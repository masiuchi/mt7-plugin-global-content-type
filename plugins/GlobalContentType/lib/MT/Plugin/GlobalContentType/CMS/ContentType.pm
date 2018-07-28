package MT::Plugin::GlobalContentType::CMS::ContentType;
use strict;
use warnings;

use MT;
use MT::CMS::ContentType;
use MT::Util;

sub can_delete {
    my ( $eh, $app, $obj ) = @_;
    my $user = $app->user or return;
    my $blog_id = $app->blog ? $app->blog->id : 0;
    my $perm = $user->permissions($blog_id);
    $user->can_do('delete_content_type')
        || ( $perm && $perm->can_do('delete_content_type') );
}

sub make_menu {
    my ( $content_type, $menu_order ) = @_;

    my $menu_key = 'content_data:' . $content_type->id;

    my $menu_value = {
        label        => $content_type->name,
        no_translate => { label => 1 },
        mode         => 'list',
        args         => {
            _type => 'content_data',
            type  => 'content_data_' . $content_type->id,
        },
        order     => $menu_order,
        view      => [ 'website', 'blog' ],
        condition => sub {
            my $app = MT->instance;
            return 1
                if ( $app->user->is_superuser
                || $app->user->can_manage_content_data );

            return 1 if $app->can_do('edit_all_content_data');

            my $blog = $app->blog;
            my $blog_ids
                = !$blog         ? undef
                : $blog->is_blog ? [ $blog->id ]
                :   [ $blog->id, map { $_->id } @{ $blog->blogs } ];

            my $iter = MT->model('permission')->load_iter(
                {   author_id => $app->user->id,
                    (   $blog_ids
                        ? ( blog_id => $blog_ids )
                        : ( blog_id => { not => 0 } )
                    ),
                }
            );

            my $cond;
            while ( my $p = $iter->() ) {
                $cond = 1, last
                    if $p->has(
                    'manage_content_data:' . $content_type->unique_id );

                $cond = 1, last
                    if $p->has(
                    'edit_all_content_data:' . $content_type->unique_id );

                $cond = 1, last
                    if $p->has(
                    'create_content_data:' . $content_type->unique_id );
            }
            return $cond ? 1 : 0;
        },
    };

    ( $menu_key, $menu_value );
}

sub save {
    my ($app) = @_;
    my $cfg   = $app->config;
    my $user  = $app->user;

    my %param = ();
    for my $col (qw{ name description user_disp_option label_field data }) {
        $param{$col} = $app->param($col);
    }

    # Permission Check
    $app->validate_magic
        or return $app->errtrans("Invalid request.");
    my $perms = $app->permissions
        or return $app->permission_denied();

    my $content_type_id = $app->param('id');

    return $app->permission_denied
        unless _can_save( undef, $app, $content_type_id );

    return $app->errtrans("Invalid request.")
        if $app->param('blog_id');

    my $blog_id = 0;

    # Load or create object
    my ( $obj, $orig_obj );
    my $ct_class = MT->model('content_type');
    if ($content_type_id) {
        $obj = $ct_class->load($content_type_id)
            or return $app->error(
            $app->translate(
                'Cannot load content type #[_1]',
                $content_type_id
            )
            );
        return $app->error( $app->translate('Invalid parameter') )
            unless $obj->blog_id == $blog_id;
        $orig_obj = $obj->clone;
    }
    else {
        $obj      = $ct_class->new;
        $orig_obj = $obj->clone;
    }

    # Validation for content type
    my $name = $app->param('name');

    if ( !$name ) {
        $param{error} = $app->translate("The content type name is required.");
        $app->mode('view');
        return $app->forward( "view", \%param );
    }
    elsif ( length $name > 255 ) {
        $param{error} = $app->translate(
            "The content type name must be shorter than 255 characters.");
        $app->mode('view');
        return $app->forward( "view", \%param );
    }

    # Duplication check
    my $exists = $ct_class->count(
        {   name    => $name,
            blog_id => $blog_id,
            ( $content_type_id ? ( id => $content_type_id ) : () ),
        },
        { ( $content_type_id ? ( not => { id => 1 } ) : () ) }
    );
    if ($exists) {
        $param{error}
            = $app->translate( 'Name \'[_1]\' is already used.', $name );
        $app->mode('view');
        return $app->forward( "view", \%param );
    }

    # Content Fields
    my $field_list;
    if ( my $data = $app->param('data') ) {
        if ( $data =~ /^".*"$/ ) {
            $data =~ s/^"//;
            $data =~ s/"$//;
            $data = MT::Util::decode_js($data);
        }
        my $decode = JSON->new->utf8(0);
        $field_list = $decode->decode($data);
    }
    else {
        $field_list = [];
    }

    # Prepare save field data
    my @field_objects       = ();
    my $cf_class            = MT->model('content_field');
    my $content_field_types = $app->registry('content_field_types');
    my $data_label_field    = $app->param('label_field') || '';
    foreach my $field (@$field_list) {
        my $type     = $field->{type};
        my $field_id = $field->{id};

        if ( !exists $content_field_types->{$type} ) {
            $type =~ s/-/_/g;
            $field->{type} = $type;
        }

        # Validation
        if (my $err_msg
            = MT::CMS::ContentType::_validate_content_field_type_options(
                $app, $field
            )
            )
        {
            $param{error} = $err_msg;
            $app->mode('view');
            return $app->forward( "view", \%param );
        }

        # Create or load content field
        my $content_field;
        if ( $content_type_id && $field_id ) {
            $content_field = $cf_class->load($field_id)
                or return $app->errtrans(
                "Cannot load content field data (ID: [_1])", $field_id );
            $field->{label_field} = 1
                if $data_label_field
                && $data_label_field eq $content_field->unique_id;
        }
        else {
            $content_field = $cf_class->new;
            $content_field->blog_id($blog_id);
            $content_field->type($type);
            $field->{label_field} = 1
                if $data_label_field
                && $data_label_field eq $field->{options}->{id};
        }

        $content_field->name( $field->{options}->{label} );
        $content_field->description( $field->{description} );
        $content_field->required( $field->{required} );

        # Pre save manipurator
        my $options = $field->{options};
        delete $options->{id} if exists $options->{id};

        if ( my $pre_save
            = $content_field_types->{$type}{options_pre_save_handler} )
        {
            if ( !ref $pre_save ) {
                $pre_save = MT->handler_to_coderef($pre_save);
            }
            if ( 'CODE' eq ref $pre_save ) {
                $pre_save->( $app, $type, $content_field, $options );
                $field->{options} = $options;
            }
        }

        # Push content field object
        push @field_objects,
            {
            object => $content_field,
            data   => $field,
            };
    }

    # Update content type object
    my $description = $app->param('description');
    my $display_option
        = $obj->id ? $app->param('user_disp_option') ? 1 : 0 : 1;
    $obj->blog_id($blog_id);
    $obj->name($name);
    $obj->description($description);
    $obj->user_disp_option($display_option);

    # Save content fields
    my @field_data = ();
    my $set_data_label;
    foreach my $field_data (@field_objects) {
        my $content_field = $field_data->{object};
        my $field         = $field_data->{data};

        $content_field->modified_by( $user->id ) if $content_field->id;
        $content_field->save
            or return $app->error(
            $app->translate(
                "Saving content field failed: [_1]",
                $content_field->errstr
            )
            );

        my $type_label = $content_field_types->{ $field->{type} }->{label};
        $type_label = $type_label->() if 'CODE' eq ref $type_label;
        my $store_data = {
            id         => $content_field->id,
            unique_id  => $content_field->unique_id,
            order      => $field->{order},
            type       => $field->{type},
            type_label => $type_label,
            options    => $field->{options},
        };
        push @field_data, $store_data;

        if ( $field->{label_field} ) {
            $obj->data_label( $content_field->unique_id );
            $set_data_label = 1;
        }
    }

    # Delete data_label if content field is not found in this type
    $obj->data_label(undef) unless $set_data_label;

    # Remove fields
    if ($content_type_id) {
        my @field_ids = map { $_->{object}->id } @field_objects;

        my $iter = $cf_class->load_iter(
            {   content_type_id => $content_type_id,
                ( @field_ids ? ( id => \@field_ids ) : () ),
            },
            { ( @field_ids ? ( not => { id => 1 } ) : () ), }
        );
        while ( my $content_field = $iter->() ) {
            if (   $content_field->type eq 'date_and_time'
                || $content_field->type eq 'date_only' )
            {
                if ( MT->model('templatemap')
                    ->count( { dt_field_id => $content_field->id } ) )
                {
                    $app->add_return_arg( not_deleted => 1 );
                    return $app->call_return;
                }
            }
            elsif ( $content_field->type eq 'categories' ) {
                if ( MT->model('templatemap')
                    ->count( { cat_field_id => $content_field->id } ) )
                {
                    $app->add_return_arg( not_deleted => 1 );
                    return $app->call_return;
                }
            }
        }

        $cf_class->remove(
            {   content_type_id => $content_type_id,
                ( @field_ids ? ( id => \@field_ids ) : () ),
            },
            { ( @field_ids ? ( not => { id => 1 } ) : () ), }
        );
    }

    $obj->fields( \@field_data );

    $app->run_callbacks( 'cms_pre_save.content_type', $app, $obj, $orig_obj )
        || return $app->error(
        $app->translate(
            "Saving [_1] failed: [_2]", $ct_class->class_label,
            $app->errstr
        )
        );

    $obj->modified_by( $user->id ) if $obj->id;

    $obj->save
        or return $app->error(
        $app->translate( "Saving content type failed: [_1]", $obj->errstr ) );

    $app->run_callbacks( 'cms_post_save.content_type', $app, $obj,
        $orig_obj );

    # Set content_type id for each content_field
    foreach my $field_data (@field_objects) {
        my $content_field = $field_data->{object};
        $content_field->content_type_id( $obj->id );
        $content_field->save
            or return $app->error(
            $app->translate(
                "Saving content field failed: [_1]",
                $content_field->errstr
            )
            );
    }

    return $app->redirect(
        $app->uri(
            'mode' => 'view',
            args   => {
                blog_id => $blog_id,
                _type   => 'content_type',
                id      => $obj->id,
                saved   => 1,
            }
        )
    );
}

sub _can_save {
    my ( $eh, $app, $id ) = @_;
    my $user = $app->user or return;
    return if $app->blog;

    my $perm = $user->permissions(0);

    return 1
        if $user->can_do('edit_all_content_types')
        || ( $perm && $perm->can_do('edit_all_content_types') );

    return 1
        if !$id
        && ( $user->can_do('create_new_content_type')
        || ( $perm && $perm->can_do('create_new_content_type') ) );

    0;
}

sub make_ct_cf_selects {
    my ($app) = @_;
    my $blog = $app->blog;
    return unless $blog && $blog->id;

    my $obj = MT->model('template')->load( $app->param('id') || 0 );

    my @content_types
        = MT->model('content_type')->load( { blog_id => [ 0, $blog->id ] } );

    my @ct_selects = ();
    my $ct_data    = {};
    my $cf_selects = {};
    my $cf_data    = {};
    foreach my $ct (@content_types) {

        # Content Type
        push @ct_selects,
            {
            id       => $ct->id,
            label    => $ct->name,
            selected => (
                       $obj
                    && $obj->content_type_id
                    && $obj->content_type_id == $ct->id ? 1 : 0
            )
            };
        $ct_data->{ $ct->id } = {
            id        => $ct->id,
            label     => $ct->name,
            unique_id => $ct->unique_id,
        };

        # Content Field
        my $fields = $ct->fields;
        my @cfs    = MT->model('content_field')
            ->load( { content_type_id => $ct->id } );
        foreach my $cf (@cfs) {
            my ($field) = grep { $_->{id} == $cf->id } @{$fields};
            my $label = $field->{options}{label};
            push @{ $cf_selects->{ $ct->id } },
                { id => $cf->id, label => $cf->name };
            my $content_field_types = $app->registry('content_field_types');
            my $type_label = $content_field_types->{ $cf->type }->{label};
            $type_label = $type_label->()
                if 'CODE' eq ref $type_label;
            $cf_data->{ $cf->id } = {
                id        => $cf->id,
                label     => $label,
                unique_id => $cf->unique_id,
                type      => $type_label,
            };
        }
    }

    +{  ct_selects => \@ct_selects,
        ct_data    => MT::Util::to_json($ct_data),
        cf_selects => MT::Util::to_json($cf_selects),
        cf_data    => MT::Util::to_json($cf_data),
    };
}

1;


package MT::Plugin::GlobalContentType::CMS::ContentData;
use strict;
use warnings;

use JSON  ();
use POSIX ();

use MT;
use MT::ContentStatus;
use MT::Serialize;
use MT::Util;

sub edit {
    my ( $app, $param ) = @_;
    my $blog = $app->blog;
    my $user = $app->user;
    my $cfg  = $app->config;
    my $data;

    unless ($blog) {
        return $app->return_to_dashboard( redirect => 1 );
    }

    $param ||= {};

    # Parameter check
    my $content_type_id = $app->param('content_type_id')
        or return $app->errtrans("Invalid request.");
    my $content_type = MT->model('content_type')->load($content_type_id)
        or return $app->errtrans('Invalid request.');

    # Permission check
    my $perm = $app->permissions
        or return $app->permission_denied();

    my $content_data_id = $app->param('id');
    if ( !$content_data_id ) {
        return $app->permission_denied()
            unless $perm->can_do('create_new_content_data')
            || $perm->can_do(
            'create_new_content_data_' . $content_type->unique_id );
        $param->{can_create_this} = 1;
    }
    else {
        return $app->permission_denied()
            unless $perm->can_edit_content_data( $content_data_id, $user );
    }

    $param->{can_create_this} = 1
        if $perm->can_do('create_new_content_data')
        || $perm->can_do(
        'create_new_content_data_' . $content_type->unique_id );

    if ( $app->param('_recover') && !$app->param('reedit') ) {
        $app->param( '_type', 'content_data' );
        my $sess_obj = $app->autosave_session_obj;
        if ($sess_obj) {
            my $autosave_data = $sess_obj->thaw_data;
            if ($autosave_data) {
                $app->param( $_, $autosave_data->{$_} )
                    for keys %$autosave_data;
                my $id = $app->param('id');
                $app->delete_param('id')
                    if defined $id && !$id;
                $data = $autosave_data->{data};
                $param->{'recovered_object'} = 1;
            }
            else {
                $param->{'recovered_failed'} = 1;
            }
        }
        else {
            $param->{'recovered_failed'} = 1;
        }
    }

    if ( $app->param('reedit') ) {
        $data = $app->param('serialized_data');
        unless ( ref $data ) {
            $data = JSON::decode_json($data);
        }
        if ($data) {
            $app->param( $_, $data->{$_} ) for keys %$data;
        }
        $app->param( had_error => 1 ) if $param->{err_msg};
    }
    else {
        $app->param( '_type', 'content_data' );
        if ( my $sess_obj = $app->autosave_session_obj ) {
            $param->{autosaved_object_exists} = 1;
            $param->{autosaved_object_ts}
                = MT::Util::epoch2ts( $blog, $sess_obj->start );
        }
    }

    $param->{autosave_frequency} = $app->config->AutoSaveFrequency;
    $param->{name}               = $content_type->name;
    $param->{has_multi_line_text_field}
        = $content_type->has_multi_line_text_field;

    my $array        = $content_type->fields;
    my $ct_unique_id = $content_type->unique_id;

    $param->{use_revision} = $blog->use_revision ? 1 : 0;

    my $content_data;
    if ($content_data_id) {
        $content_data = MT->model('content_data')->load(
            {   id              => $content_data_id,
                content_type_id => $content_type->id,
            }
            )
            or return $app->error(
            $app->translate(
                'Load failed: [_1]',
                MT->model('content_data')->errstr
                    || $app->translate('(no reason given)')
            )
            );

        if ( $blog->use_revision ) {

            my $original_revision = $content_data->revision;
            my $rn                = $app->param('r');
            if ( defined $rn && $rn != $content_data->current_revision ) {
                my $status_text
                    = MT::ContentStatus::status_text( $content_data->status );
                $param->{current_status_text} = $status_text;
                $param->{current_status_label}
                    = $app->translate($status_text);
                my $rev
                    = $content_data->load_revision( { rev_number => $rn } );
                if ( $rev && @$rev ) {
                    $content_data = $rev->[0];
                    my $values = $content_data->get_values;
                    $param->{$_} = $values->{$_} for keys %$values;
                    $param->{loaded_revision} = 1;
                }
                $param->{rev_number} = $rn;
                $param->{no_snapshot} = 1 if $app->param('no_snapshot');
            }
            $param->{rev_date} = MT::Util::format_ts(
                '%Y-%m-%d %H:%M:%S',
                $content_data->modified_on,
                $blog, $app->user ? $app->user->preferred_language : undef
            );
        }

        $param->{identifier}
            = $app->param('identifier') || $content_data->identifier;

        my $status = $app->param('status') || $content_data->status;
        $status =~ s/\D//g;
        $param->{status} = $status;
        $param->{ 'status_' . MT::ContentStatus::status_text($status) } = 1;

        $param->{content_data_permalink}
            = MT::Util::encode_html( $content_data->permalink );

        $param->{authored_on_date} = $app->param('authored_on_date')
            || MT::Util::format_ts( '%Y-%m-%d', $content_data->authored_on,
            $blog, $app->user ? $app->user->preferred_language : undef );
        $param->{authored_on_time} = $app->param('authored_on_time')
            || MT::Util::format_ts( '%H:%M:%S', $content_data->authored_on,
            $blog, $app->user ? $app->user->preferred_language : undef );
        $param->{unpublished_on_date} = $app->param('unpublished_on_date')
            || MT::Util::format_ts( '%Y-%m-%d', $content_data->unpublished_on,
            $blog, $app->user ? $app->user->preferred_language : undef );
        $param->{unpublished_on_time} = $app->param('unpublished_on_time')
            || MT::Util::format_ts( '%H:%M:%S', $content_data->unpublished_on,
            $blog, $app->user ? $app->user->preferred_language : undef );
    }
    else {
        my $def_status;
        if ( $def_status = $app->param('status') ) {
            $def_status =~ s/\D//g;
            $param->{status} = $def_status;
        }
        else {
            $def_status = $blog->status_default;
        }
        $param->{ "status_" . MT::ContentStatus::status_text($def_status) }
            = 1;

        my @now = MT::Util::offset_time_list( time, $blog );
        $param->{authored_on_date} = $app->param('authored_on_date')
            || POSIX::strftime( '%Y-%m-%d', @now );
        $param->{authored_on_time} = $app->param('authored_on_time')
            || POSIX::strftime( '%H:%M:%S', @now );
        $param->{unpublished_on_date} = $app->param('unpublished_on_date');
        $param->{unpublished_on_time} = $app->param('unpublished_on_time');
    }

    $data = $content_data->data if $content_data && !$data;
    my $convert_breaks
        = $content_data
        ? MT::Serialize->unserialize( $content_data->convert_breaks )
        : undef;

    my $content_field_types = $app->registry('content_field_types');
    @$array = map {
        my $e_unique_id = $_->{unique_id};
        my $can_edit_field
            = $app->permissions->can_do( 'content_type:'
                . $ct_unique_id
                . '-content_field:'
                . $e_unique_id );
        if (   $can_edit_field
            || $app->permissions->can_do('edit_all_content_data') )
        {
            $_->{can_edit} = 1;
        }
        $_->{content_field_id} = $_->{id};
        delete $_->{id};

        if ( $app->param( $_->{content_field_id} ) ) {
            $_->{value} = $app->param( $_->{content_field_id} );
        }
        elsif ( $content_data_id || $data ) {
            $_->{value} = $data->{ $_->{content_field_id} };
        }
        else {
            # TODO: fix after updating values option.
            if ( $_->{type} eq 'select_box' || $_->{type} eq 'checkboxes' ) {
                my $delimiter = quotemeta( $_->{options_delimiter} || ',' );
                my @values = split $delimiter,
                    ( $_->{options}{initial_value} || '' );
                $_->{value} = \@values;
            }
            else {
                $_->{value} = $_->{options}{initial_value};
            }
        }

        my $content_field_type = $content_field_types->{ $_->{type} };

        if ( my $field_html_params
            = $content_field_type->{field_html_params} )
        {
            if ( !ref $field_html_params ) {
                $field_html_params
                    = MT->handler_to_coderef($field_html_params);
            }
            if ( 'CODE' eq ref $field_html_params ) {
                $field_html_params = $field_html_params->( $app, $_ );
            }

            if ( ref $field_html_params eq 'HASH' ) {
                for my $key ( keys %{$field_html_params} ) {
                    unless ( exists $_->{$key} ) {
                        $_->{$key} = $field_html_params->{$key};
                    }
                }
            }
        }

        if ( my $field_html = $content_field_type->{field_html} ) {
            if ( !ref $field_html ) {
                if ( $field_html =~ /\.tmpl$/ ) {
                    my $plugin = $content_field_type->{plugin};
                    $field_html
                        = $plugin->id eq 'core'
                        ? $app->load_tmpl($field_html)
                        : $plugin->load_tmpl($field_html);
                    $field_html = $field_html->text if $field_html;
                }
                else {
                    $field_html = MT->handler_to_coderef($field_html);
                }
            }
            if ( 'CODE' eq ref $field_html ) {
                $_->{field_html} = $field_html->( $app, $_ );
            }
            else {
                $_->{field_html} = $field_html;
            }
        }

        $_->{data_type} = $content_field_types->{ $_->{type} }{data_type};
        if ( $_->{type} eq 'multi_line_text' ) {
            if ( $convert_breaks
                && exists $$convert_breaks->{ $_->{content_field_id} } )
            {
                $_->{convert_breaks}
                    = $$convert_breaks->{ $_->{content_field_id} };
            }
            elsif ( $content_data_id || $data ) {
                my $key
                    = 'content-field-'
                    . $_->{content_field_id}
                    . '_convert_breaks';
                $_->{convert_breaks} = $app->param($key);
            }
            else {
                $_->{convert_breaks} = $_->{options}{input_format};
            }
        }
        $_;
    } @$array;

    $param->{fields} = $array;

    foreach
        my $name (qw( saved_added saved_changes err_msg content_type_id id ))
    {
        $param->{$name} = $app->param($name) if $app->param($name);
    }

    $param->{new_object}          = $content_data_id ? 0 : 1;
    $param->{object_label}        = $content_type->name;
    $param->{sitepath_configured} = $blog && $blog->site_path ? 1 : 0;

    if ( $content_type->data_label ) {
        $param->{can_edit_data_label} = 0;
        if ($content_data_id) {
            $param->{data_label}
                = $app->param('data_label') || $content_data->label;
        }
        else {
            my $field = MT->model('content_field')->load(
                {   content_type_id => $content_type->id,
                    unique_id       => $content_type->data_label,
                }
                )
                or die MT->translate(
                'Cannot load content field (UniqueID:[_1]).',
                $content_type->data_label );
            $param->{data_label}
                = $app->translate(
                'The value of [_1] is automatically used as a data label.',
                $field->name );
        }
    }
    else {
        $param->{can_edit_data_label} = 1;
        $param->{data_label}          = $app->param('data_label')
            || ( $content_data_id ? $content_data->label : '' );
    }

    $param->{can_publish_post} = 1
        if ( $perm->can_do('publish_all_content_data')
        || $perm->can_do('edit_all_content_data_$ct_unique_id') )
        || ( $content_data
        || $perm->can_republish_content_data( $content_data, $user ) );

    ## Load text filters if user displays them
    my $filters = MT->all_text_filters;
    $param->{text_filters} = [];
    for my $filter ( keys %$filters ) {
        if ( my $cond = $filters->{$filter}{condition} ) {
            $cond = MT->handler_to_coderef($cond) if !ref($cond);
            next unless $cond->('content-type');
        }
        push @{ $param->{text_filters} },
            {
            filter_key   => $filter,
            filter_label => $filters->{$filter}{label},
            filter_docs  => $filters->{$filter}{docs},
            };
    }
    $param->{text_filters} = [ sort { $a->{filter_key} cmp $b->{filter_key} }
            @{ $param->{text_filters} } ];
    unshift @{ $param->{text_filters} },
        {
        filter_key   => '0',
        filter_label => $app->translate('None'),
        };

    $app->setup_editor_param($param);

    $param->{basename_limit} = ( $blog ? $blog->basename_limit : 0 ) || 30;

    $app->build_page( $app->load_tmpl('edit_content_data.tmpl'), $param );
}

sub delete {
    my $app = shift;

    return unless $app->validate_magic;

    my $blog;
    if ( my $blog_id = $app->param('blog_id') ) {
        $blog = MT->model('blog')->load($blog_id)
            or return $app->error(
            $app->translate( 'Cannot load blog #[_1].', $blog_id || '(none' )
            );
    }

    my $content_type_id = $app->param('content_type_id');
    unless ($content_type_id) {
        my $type = $app->param('type') || '';
        if ( $type =~ /^content_data_(\d+)$/ ) {
            $content_type_id = $1;
        }
    }
    my $content_type;
    if ($content_type_id) {
        $content_type = MT->model('content_type')->load($content_type_id);
    }
    unless ($content_type) {
        return $app->errtrans(
            'Cannot load content_type #[_1]',
            $content_type_id || '(none)'
        );
    }

    my $can_background
        = ( ( $blog && $blog->count_static_templates('ContentType') == 0 )
            || MT::Util->launch_background_tasks() ) ? 1 : 0;

    $app->setup_filtered_ids
        if $app->param('all_selected');
    my %rebuild_recipe;
    for my $id ( $app->multi_param('id') ) {
        my $class = $app->model('content_data');
        my $obj   = $class->load($id);
        return $app->call_return unless $obj;

        $app->run_callbacks( 'cms_delete_permission_filter.content_data',
            $app, $obj )
            or return $app->permission_denied;

        my %recipe;
        %recipe = $app->publisher->rebuild_deleted_content_data(
            ContentData => $obj,
            Blog        => $obj->blog,
        ) if $obj->status eq MT::ContentStatus::RELEASE();

        # Remove object from database
        my $content_type_name
            = defined $content_type->name && $content_type->name ne ''
            ? $content_type->name
            : $app->translate('Content Data');
        $obj->remove()
            or return $app->errtrans( 'Removing [_1] failed: [_2]',
            $content_type_name, $obj->errstr );
        $app->run_callbacks( 'cms_post_delete.content_data', $app, $obj );

        my $child_hash = $rebuild_recipe{ $obj->blog_id } || {};
        MT::__merge_hash( $child_hash, \%recipe );
        $rebuild_recipe{ $obj->blog_id } = $child_hash;

        # Clear cache for site stats dashboard widget.
        MT::Util::clear_site_stats_widget_cache( $obj->blog->id )
            or return $app->errtrans('Removing stats cache failed.');
    }

    $app->add_return_arg( saved_deleted => 1 );

    if ( $app->config('RebuildAtDelete') ) {
        $app->run_callbacks('pre_build');

        my $rebuild_func = sub {
            foreach my $b_id ( keys %rebuild_recipe ) {
                my $b   = MT->model('blog')->load($b_id);
                my $res = $app->rebuild_archives(
                    Blog   => $b,
                    Recipe => $rebuild_recipe{$b_id},
                ) or return $app->publish_error();
                $app->rebuild_indexes( Blog => $b )
                    or return $app->publish_error();
                $app->run_callbacks( 'rebuild', $b );
            }
        };

        if ($can_background) {
            MT::Util::start_background_task($rebuild_func);
        }
        else {
            $rebuild_func->();
        }

        $app->add_return_arg( no_rebuild => 1 );
        my %params = (
            is_full_screen  => 1,
            redirect_target => $app->base
                . $app->path
                . $app->script . '?'
                . $app->return_args,
        );
        return $app->load_tmpl( 'rebuilding.tmpl', \%params );
    }

    return $app->call_return;

}

1;


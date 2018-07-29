package MT::Plugin::GlobalContentType::Callback;
use strict;
use warnings;

use feature 'state';

use MT;
use MT::Plugin::GlobalContentType::CMS::ContentData;
use MT::Plugin::GlobalContentType::CMS::ContentType;

sub init_app {
    _override_cs_single_select_options();

    _override_edit_content_type();
    _override_save_content_type();
    _override_can_delete_content_type();

    _override_edit_content_data();
    _override_delete_content_data();
}

sub cms_pre_load_filtered_list {
    my ( $cb, $app, $filter, $options, $cols ) = @_;
    return if $app->blog && $app->blog->id;
    $options->{terms}{blog_id} = 0;
}

sub post_init {
    my $iter
        = MT->model('content_type')->load_iter( { blog_id => 0 } );
    while ( my $content_type = $iter->() ) {
        _update_listing_screens_for_global_content_type($content_type);
        _add_menus_for_global_content_type($content_type);
    }
}

sub tmpl_param_edit_template {
    my ( $cb, $app, $param, $tmpl ) = @_;
    return unless $app->blog && $app->blog->id;

    my $data
        = MT::Plugin::GlobalContentType::CMS::ContentType::make_ct_cf_selects(
        $app);
    $param->{ct_selects} = $data->{ct_selects};
    $param->{ct_data}    = $data->{ct_data};
    $param->{cf_selects} = $data->{cf_selects};
    $param->{cf_data}    = $data->{cf_data};

    if ( $param->{type} eq 'ct' || $param->{type} eq 'ct_archive' ) {
        $param->{content_types} = [ $app->model('content_type')
                ->load( { blog_id => [ 0, $app->blog->id ] } ) ];
    }
}

sub _update_listing_screens_for_global_content_type {
    my ($content_type) = @_;
    my $registry       = MT->component('core')->registry('listing_screens');
    my $key            = 'content_data.content_data_' . $content_type->id;
    $registry->{$key}{condition} = sub {
        my ($app) = @_;
        ( $app->blog && $app->blog->id ) ? 1 : 0;
    };
}

sub _add_menus_for_global_content_type {
    my ($content_type) = @_;

    state $menu_order = 0;

    my ( $menu_key, $menu_value )
        = MT::Plugin::GlobalContentType::CMS::ContentType::make_menu(
        $content_type, $menu_order );
    MT->component('core')->registry( 'menus', $menu_key, $menu_value );

    $menu_order++;
}

sub _override_cs_single_select_options {
    require MT::ContentType;
    my $_cs_single_select_options
        = \&MT::ContentType::_cs_single_select_options;
    no warnings 'redefine';
    *MT::ContentType::_cs_single_select_options = sub {
        my $app = MT->app;
        local $app->{_blog} = MT->model('blog')->new( id => 0 )
            unless $app->blog;
        $_cs_single_select_options->(@_);
    };
}

sub _override_edit_content_type {
    require MT::CMS::ContentType;
    my $edit = \&MT::CMS::ContentType::edit;
    no warnings 'redefine';
    *MT::CMS::ContentType::edit = sub {
        my ($app) = @_;
        local $app->{_blog} = MT->model('website')->new( id => 0 )
            unless $app->blog;
        $edit->(@_);
    };
}

sub _override_save_content_type {
    require MT::CMS::ContentType;
    my $save = \&MT::CMS::ContentType::save;
    no warnings 'redefine';
    *MT::CMS::ContentType::save = sub {
        my ($app) = @_;
        if ( $app->blog ) {
            $save->(@_);
        }
        else {
            MT::Plugin::GlobalContentType::CMS::ContentType::save(@_);
        }
    };
}

sub _override_can_delete_content_type {
    require MT::CMS::ContentType;
    my $can_delete = \&MT::CMS::ContentType::can_delete;
    no warnings 'redefine';
    *MT::CMS::ContentType::can_delete = sub {
        my ( $eh, $app, $obj ) = @_;
        if ( $obj->blog_id ) {
            $can_delete->(@_);
        }
        else {
            MT::Plugin::GlobalContentType::CMS::ContentType::can_delete(@_);
        }
    };
}

sub _override_edit_content_data {
    require MT::CMS::ContentData;
    my $edit = \&MT::CMS::ContentData::edit;
    no warnings 'redefine';
    *MT::CMS::ContentData::edit = sub {
        my ($app) = @_;
        my ($content_type_id)
            = $app->param('content_type_id')
            ? ( scalar $app->param('content_type_id') )
            : $app->param('type') =~ /^content_data_([0-9]+)$/;
        my $content_type
            = $app->model('content_type')->load( $content_type_id || 0 );
        if ( $content_type && !$content_type->blog_id ) {
            MT::Plugin::GlobalContentType::CMS::ContentData::edit(@_);
        }
        else {
            $edit->(@_);
        }
    };
}

sub _override_delete_content_data {
    require MT::CMS::ContentData;
    my $delete = \&MT::CMS::ContentData::delete;
    no warnings 'redefine';
    *MT::CMS::ContentData::delete = sub {
        my ($app) = @_;
        my $content_data_id = $app->param('id');
        my $content_data
            = $app->model('content_data')->load( $content_data_id || 0 );
        my $content_type = $app->model('content_type')
            ->load( $content_data ? $content_data->content_type_id : 0 );
        if ( $content_type && !$content_type->blog_id ) {
            MT::Plugin::GlobalContentType::CMS::ContentData::delete(@_);
        }
        else {
            $delete->(@_);
        }
    };
}

1;


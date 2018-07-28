use strict;
use warnings;

use Test::More;

use FindBin;
use lib "$FindBin::Bin/../../../lib", "$FindBin::Bin/../../../extlib",
    "$FindBin::Bin/../lib";

use_ok('MT::Plugin::GlobalContentType::CMS::ContentData');
use_ok('MT::Plugin::GlobalContentType::CMS::ContentType');
use_ok('MT::Plugin::GlobalContentType::Callback');

done_testing;


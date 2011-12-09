#
# $Id: 01-use.t,v 0.1 2003/05/31 10:43:20 dankogai Exp $
#
use strict;
use warnings;
use Test::More tests => 2;
use_ok('Regexp::Optimizer');
my $o = new Regexp::Optimizer;
isa_ok($o,'Regexp::List');

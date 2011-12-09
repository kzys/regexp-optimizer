#!/usr/local/bin/perl
use strict;
use Regexp::List;
use Getopt::Std;

my %Opt;
getopts("r:e:" => \%Opt);
my $rx = do  $Opt{r};
$@ and die $@;
my $e = $Opt{e} ? eval qq{ sub { $Opt{e} } }  : sub { qq/[$_[0]]/ };

while(<>){
    s/($rx)/$e->($1)/eg;
    print;
}

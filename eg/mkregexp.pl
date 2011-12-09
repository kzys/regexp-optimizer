#!/usr/local/bin/perl
#
# $Id: mkregexp.pl,v 0.1 2003/05/31 10:43:20 dankogai Exp $
#
use strict;
use Regexp::List;
use Getopt::Std;

my %Opt;
getopts("a:dixw:" => \%Opt);
my $re_word = 
    $Opt{w} ? qr/$Opt{w}/ : qr/[0-9A-Za-z_\x80-\xff]+/o;
my $sub = '';
if ($Opt{a}){
    $sub = eval qq{ sub { $Opt{a} } };
    $@ and die "$Opt{a} : $@";
}

my %words;

unless ($Opt{a}){
    while(<>){
	s/($re_word)/$words{$1}++/ego;
    }
}else{
    for my $file (@ARGV){
	open F, $file or die "$file:$!";
	while(<F>){
	    s/($re_word)/$words{$1}++/ego;
	}
    }
}
#use Data::Dumper;
#warn join "\n",  sort keys %words;

my $modifiers;
$Opt{i} and $modifiers .= 'i';
$Opt{x} and $modifiers .= 'x';

my $l2r = Regexp::List->new(
			   debug     =>  $Opt{d},
			   as_string => !$Opt{a},
			   # sort => 1,
			   optim_sx => 1,
			   optim_px => 1,
			   modifiers =>  $modifiers,
			  );
my $regex = $l2r->list2regex(keys %words);
unless ($Opt{a}){
    print "qr/$regex/\n";
}else{
    for my $file (@ARGV){
	open F, $file or die "$file:$!";
	while(<F>){
	    s/($regex)/$sub->($1)/ego;
	    print; 
	}
    }
}

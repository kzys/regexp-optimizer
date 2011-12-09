#
# $Id: 00-dict.pl,v 0.1 2003/05/31 10:43:20 dankogai Exp $
#
use strict;
# use Test::More qw/no_plan/;
use Regexp::List;
use Getopt::Std;
use File::Basename;
use File::Spec;
use Time::HiRes qw/time/;

my $dict_file   = shift or die;
my $regexp_file = File::Spec->catfile('t', basename($dict_file) . '.rx');
my @words;
my ($now, $then, $total);

sub comment{
    my ($msg, $check) = @_;
    $now = time(); 
    $check and $then = $now;
    printf "# %s (%13.6f s) $msg\n", scalar(localtime($now)), $now - $then;
}

unless (-f $regexp_file){
    comment "Reading $dict_file", 1;
    open DICT, $dict_file or die "$dict_file:$!";
    my $nlines = 0;
    while(<DICT>){
	chomp;
	push @words, $_;
	$nlines++;
    }
    comment "$nlines lines read.";
    comment "Making regexp.", 1;
    my $l = Regexp::List->new;
    my $regexp = $l->list2regex(@words);
    comment "Done.";
    comment "Saving to $regexp_file", 1;
    open RX, ">", $regexp_file or die "$regexp_file:$!";
    print RX 'qr/' . $regexp . '/';
    close RX;
    undef $regexp;
}

comment "Reading $regexp_file", 1;
my $regexp = do $regexp_file;
comment ($@ ? "FAILED: $@" : "Done.");

comment "Opening $dict_file for comparison.", 1;
open DICT, $dict_file or die "$dict_file:$!";
my $nlines = 0;
$nlines++ while(<DICT>);
comment "$dict_file:$nlines lines found.";
seek DICT, 0, 0;
comment "Showtime!", 1;
my $line = 0; $| = 1;
while(<DICT>){
    m/($regexp)/;
    chomp;
    $_ eq $1 or die "$_ did not match! (word = $_, match = $1)";
    printf "\r# %d/%d", ++$line, $nlines;
}
print "\n";
comment "Done.";
comment sprintf("%.3f matches/s", $nlines/($now-$then))
		

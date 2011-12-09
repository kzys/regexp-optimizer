#
# $Id: 03-utf8.t,v 0.3 2003/06/02 20:11:54 dankogai Exp $
#
BEGIN {
    if ($ENV{'PERL_CORE'}){
        chdir 't';
        unshift @INC, '../lib';
    }
    if ($] < 5.008 ) {
      print "1..0 # Skip: Perl 5.8 or better needed. \n";
      exit 0;
  }
    require Config; import Config;
    if ($Config{'extensions'} !~ /\bEncode\b/) {
      print "1..0 # Skip: Encode not available. \n";
      exit 0;
  }
}
use strict;
use warnings;
use Test::More tests => 21;
use Regexp::Optimizer;
my $l = Regexp::List->new(lookahead => 0);
my $o = Regexp::Optimizer->new;
my $Debug = shift;

# not so trivial

my $qq_utf8   = qq/\x20|\x{3000}/;
my $qr_utf8_l = qq/[\\\x20\x{3000}]/;
my $qr_utf8_o = qq/[\x20\x{3000}]/;

use charnames ':full';

my %t_l = 
    (
     $qq_utf8                       => qr/$qr_utf8_l/,
     q/\x20|\x{3000}/               => qr/\\x(?:20|\{3000\})/,
     q/\p{Kanji}|\P{Hiragana}/      => qr/\\(?:p\{Kanji|P\{Hiragana)\}/,
     q/\N{DIGIT ONE}|\N{DIGIT TWO}/ => qr/\\N\{DIGIT\ (?:ONE|TWO)\}/,
     q/\N{SNOWMAN}/                 => qr/\\N\{SNOWMAN\}/,
     q/\C|\X/                       => qr/\\[CX]/,
);

my %t_o = 
    (
     $qq_utf8                       => qr/$qr_utf8_o/,
     q/\x20|\x{3000}/               => qr/[\x20\x{3000}]/,
     q/\p{Kanji}|\P{Hiragana}/      => qr/[\p{Kanji}\P{Hiragana}]/,
     q/\N{DIGIT ONE}|\N{DIGIT TWO}/ => q/(?^:[\x{31}\x{32}])/,
     q/\N{SNOWMAN}/                 => q/(?^u:\x{2603})/,
     q/\C|\X/                        => qr/(?:\C|\X)/,
    );

for (sort {length $a <=> length $b} keys %t_l){
    # no warnings qw/regexp uninitialized/;
    $Debug or local $SIG{__WARN__} = sub { };
    my @words = split /\|/, $_;
    my $regex = $_;
    is($l->list2regex(@words) => $t_l{$_}, qq/l->l($_) eq $t_l{$_}/);
    isnt($o->optimize($regex) => $t_l{$_}, qq/o->o($regex) ne $t_l{$_}/);
    is($o->optimize($regex) => $t_o{$_}, qq/o->o($regex) eq $t_o{$_}/);
}

SKIP:{
    skip "PerlIO needed" => 3 unless (PerlIO::Layer->find('perlio'));
    skip "Will work in future, hopefully!" => 3 unless 1;
    binmode STDOUT => ':utf8';
    my @words;
    use File::Spec;
    my $file = File::Spec->catfile('t', "README.utf8");
    # my $file = File::Spec->catfile('.', "foo.utf8");
    open F, "<:utf8", $file or die "$file:$!";
    while (<F>) {
	push @words, $1 while( /([^\x00-\xff]+)/gc );
    }
    close F;
    $Debug and print join(",", @words);
    my @match;
    my $re_l = $l->list2regex(@words);
    $Debug and warn $re_l;
    my $re_trivial = join('|' => map {quotemeta($_) } @words);
    my $re_o = $o->optimize($re_trivial);
    $Debug and warn $re_o;
    is($re_l, $re_o, "l->l and o->o agrees");

 SKIP:{
	skip "perl 5.8.1 or better needed" => 2 unless $] > 5.008;
	@match = ();
	open F, "<:utf8", $file or die "$file:$!";
	while (<F>) {
	    push @match, $1 while( /($re_l)/gc );
	}
	close F;
	ok(eq_array(\@words, \@match), "l->l(): all words in $file");

	@match = ();
	open F, "<:utf8", $file or die "$file:$!";
	while(<F>){
	    push @match, $1 while( /($re_o)/gc );
	}
	close F;
	ok(eq_array(\@words, \@match), "o->o(): all words in $0");
    }

}
__END__

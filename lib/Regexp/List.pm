#
# $Id: List.pm,v 0.13 2004/12/05 16:07:34 dankogai Exp dankogai $
#
package Regexp::List;
use 5.006; # qr/(??{}/ needed
use strict;
use warnings;
no warnings 'uninitialized';
#use base qw/Exporter/;
our $VERSION = do { my @r = (q$Revision: 0.13 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r };

#our @EXPORT = qw();
#our %EXPORT_TAGS = ( 'all' => [ qw() ] );
#our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our $DEBUG     = 0;

our $FILLER = "\x{fffd}"; # fallback

our $RE_START =
    qr{(?:
    (?!\\)\((?:\?
    (?:
     ([imsx\-]*:)  | # options 
     \<?[\=\!]     | # look(behind|ahead)
     \#[^\)]+      | # comments
     #$RE_PAREN    | # ( condtion )
     #\??$RE_EXPR  | # { expression }
     \>              # independent subexpression
    ))?
    )}xo;

our $RE_XCHAR =
    qr{
       (?:\\
	(?:
	[^0xclupPNLUQEXC]     | # ordinary escaped character
	 0[0-9][0-9]          | # octal
	 x(?:[0-9A-Fa-f]{1,2} | # hex
	   \{[0-9A-Fa-f]+\})  | # unicode hex
	 c.                   | # control char
	 [pP]\{\w+\}          | # unicode properties
	 N\{[\w\ ]+\}         | # unicode name
	 )
	)}xo;

our $RE_PCHAR = 
    qr{
       (?:\\
        (?:
         [XC]  # unicode name
	)
       )}xo;

our $RE_CHAR = 
    qr{(?:
        (?!\\)[^\\]            | # raw character (except \)
        $RE_XCHAR              | # extended character
       )
      }xo;

our %PARAM = 
    (
     _i         => 0,
     _m         => 0,
     _s         => 0,
     _x         => 0,
     _char      => $RE_CHAR,
     _token     => $RE_CHAR,
     _cclass    => $RE_CHAR,
     debug      => $DEBUG,
     capture    => 0,
     lookahead  => 1,
     modifiers  => '',
     optim_cc   => 1,
     optim_cq   => 1,
     optim_sx   => 1,
     po         => '(?:',
     pc         => ')',
     quotemeta  => 1, 
     sort       => 0,
     );

# aliases

*l2r         = \&list2re;
*list2regex  = \&list2re;
*list2regexp = \&list2re;

sub new{
    my $class = ref $_[0] ? ref shift : shift;
    my $self = bless { %PARAM } => $class;
    $self->set(@_);
}

sub clone{
    my $self = shift;
    my $clone = bless { %$self } => ref $self;
    $clone->set(@_);
}

sub set{
    my $self = shift;
    my %param = @_;
    for (sort keys %param){
	$self->{$_} = $param{$_};
	if ($_ eq 'capture'){
	    $self->{op} = $self->{capture} ? '(' : '(?:';
	    $self->{cp} = ')';
	}
	if ($_ eq 'modifiers'){
	    map { $self->{'_' . $_} = 0 } qw/i m s x/;
	    map { $self->{'_' . $_} = 1 } split //, $self->{$_};
	}
    }
    $self;
}

sub tokens{
    my $self  = shift;
    my $str   = shift;
    grep {$_ ne '' } split /($self->{_token})/, $str;
}

sub regopt{
    my  $re = shift;
    #ref $re eq 'Regexp' or return;
    $re =~ /^($RE_START)/ or return; # die "malformed regexp : $re";
    my $opt = $1;
    $opt =~ s/\(\?//o; $opt =~ s/[-:].*//o;
    $opt;
}

sub expand{
    my $self  = shift;
    my $re   = shift;
    my $isre  = ref $re eq 'Regexp';
    #$isre or $re = qr/$re/;
    my $mod = regopt($re);
    $mod =~ /x/ or $mod .= 'x';
    my ($indent, @indent);
    $re =~ 
	s{
	  ( $RE_START | (?!\\)[\)|])
	 }{
	     my $paren = $1;
	     my $sub = $paren;
	     if  ($paren eq ')'){ # close
		 $indent -= pop @indent;
	     }elsif($paren eq '|'){ # |
		 $sub = " | \n";
		 $sub .= " " x $indent;
	     }else{
		 $sub  = $indent ? "\n" : '';
		 $sub .= " " x $indent . $paren;
		 $indent += length($paren);
		 push @indent, length($paren);
	     }
	     $sub;
	 }geox;
    $isre ? qr/(?$mod:$re)/ : qq/(?$mod:$re)/;
}

sub unexpand{
    my $self = shift;
    my $re   = shift;
    my $isre  = ref $re eq 'Regexp';
    my $mod = regopt($re); 
    $mod =~ s/x//o;
    $re =~ s/\((?!\\)\?\#[^\)]+\)//o; # strip (?#comment)
    $re =~ s/(?!\\)#.*$//mg;          # strip comment
    $re =~ s/(?!\\)[ \t]//g;          # strip space
    # $re =~ s/([^\x00-\xff])/sprintf('\x{%04x}', ord($1))/eg;
    # and finally strip CRLF
    $re =~ s/[\n\r]//g;
    $isre ? 
	$mod ? qr/(?$mod:$re)/ : qr/$re/ :
	$mod ? qq/(?$mod:$re)/ : $re;
}

sub list2re { 
    use utf8; # for substr
    no warnings 'redefine'; # for cheats
    my $self = shift;
    # trie construction allows no duplicates 
    # so we make sure they are all unique
    my (%list, @list);
    # Unique with order preserved
    if ($self->{_i}){ for (@_){ $_=lc($_); $list{$_}++ or push @list, $_ } }
    else            { for (@_){            $list{$_}++ or push @list, $_ } }
    undef %list; # to save memory
    #$self->{sort} and @list = sort {length($b) <=> length($a) } @list;
    $self->{sort} and @list = sort @list;
    my $result;
    if ($self->{quotemeta}){
	# cheat
	*_head = sub{ substr($_[1], 0, $_[2]*2) };
	*_tail = sub{ substr($_[1], $_[2]*2) };
	$result = _trie_regex($self, map { _metaquote($_) } @list);
	$result =~ tr/\x00//d; 
	#$result =~ tr/\x{FFFd}//d;
    }else{
	*_head = \&_head_re;
	*_tail = \&_tail_re;
	$self->{_x} and @list = map { s/\\? /\\ /g; $_  } @list;
	$result = _trie_regex($self, @list); 
    }
    my $lookahead;
    if ($self->{lookahead}){
	my %lookahead;
	$lookahead{$self->_first($_)}++ for @list;
        my @lookahead = 
	    $self->{quotemeta} 
		?  map { tr/\x00//d; qq/\Q$_/ } keys %lookahead
		    #map { tr/\x{FFFd}//d; qq/\Q$_/ } keys %lookahead
		    : keys %lookahead;
	@lookahead = sort sort grep {!/^\\[luLUEQXC]/} @lookahead;
	if (@lookahead > 1){
	    my $lookahead = join('' => @lookahead);
	    $result = qq/(?=[$lookahead])$result/;
	}
	undef @lookahead;
    }
    my $mod = $self->{modifiers};
    $mod =~ 'x' and return $self->expand($result);
    $result = $self->{as_string} ? 
	$mod ? qq/(?$mod:$result)/ : qq/$result/ :
	    $mod ? qr/(?$mod:$result)/ : qr/$result/;
}

sub _metaquote{
    my $str =
	join '' => 
	    map { my $q=qq/\Q$_/; length($q) == 2 ? $q : "\x00$q" }
	    #map { my $q=qq/\Q$_/; length($q) == 2 ? $q : "\x{FFFd}$q" }
		split // => shift;
    $str;
}

sub _first{
    my $self = shift;
    my $str = shift;
    my $re = $self->{_char};
    $str =~ /^($re)/o;
    return $1;
}

sub _head_re{
    my $self = shift;
    my ($str, $pos) = @_;
    $str eq '' and return '';
    my $token = $self->{_token};
    for (my $p = $pos, pos($str) = 0; $p > 0 ; $p--){
	$str =~ /\G$token/gc or last;
    }
    substr($str, 0, pos($str));
    
}

sub _tail_re{
    use utf8;
    my $self = shift;
    my ($str, $pos) = @_;
    $str eq '' and return '';
    my $token = $self->{_token};
    for (my $p = $pos, pos($str) = 0; $p > 0 ; $p--){
	$str =~ /\G$token/gcs or last;
    }
    substr($str,pos($str));
}

use Data::Dumper;
$Data::Dumper::Indent = 1;

sub _prefixes {
    my $self = shift;
    my (@head, @prefix, %prefix);
    for (@_) {
	my $c = $self->_head($_, 1);
	exists $prefix{$c} or push @prefix, $c; # to preserve order
	$prefix{$c} ||= [];
	push @{$prefix{$c}}, $_;
    }
    for (@prefix) {
	# Find common substring
	my $prefix = $prefix{$_}->[0];
	if (@{$prefix{$_}} == 1){
	    push @head, [ $prefix ]; next 
	}
	my $l = length($prefix);
	for (@{$prefix{$_}}) {
	    $l -= 1
		while $self->_head($_, $l) ne $self->_head($prefix, $l);
	}
	# Return value
	$prefix = $self->_head($prefix, $l);
	my @suffix = map {$self->_tail($_, $l)} @{$prefix{$_}};
	push @head, [$prefix, @suffix];
    }
    #print Dumper \@head;
    #sleep 1;
    @head;
}


sub _rev{
    my $self = shift;
    my $str = shift;
    if ($self->{quotemeta}){
	return length $str > 2 ?
	    join '' => reverse split /(..)/, $str : $str;
    }else{
	my $re = $self->{_token};
	#return $str =~ /^$re?$/o ?
	#   $str : join '' => reverse split /($re)/, $str;
	$str =~ /^$self->{_token}$/ and return $str;
	my @token;
	$str =~ s{ ($re) }{ push @token, $1 }egx;
	return join '' => reverse @token;
	    
	    
    }
}
sub _trie_regex {
    my $self = shift;
    @_ or return;
    @_ == 1 and return shift;

    $self->{debug} and $self->{_indent}++;
    $self->{debug} and
	print STDERR '>'x $self->{_indent}, " ", join(',' => @_), "\n";

    my (@leaf, @result);

    #
    # Suffixing Optimization
    # - only leaf nodes in the same branch can be suffix-bundled
    #
    if ($self->{optim_sx}){
	for ($self->_prefixes(@_)){
	    my ($prefix, @suffix) = @$_;
	    if (@suffix){
		push @result, $prefix.$self->_trie_regex(@suffix);
	    }else{
		push @leaf, $prefix;
	    }
	}
	for ($self->_prefixes(map { $self->_rev($_) } @leaf)){
	    my ($suffix, @prefix) = @$_;
	    $suffix = $self->_rev($suffix);
	    if (@prefix){
		push @result, 
		    $self->_trie_regex(map { $self->_rev($_) } @prefix)
			. $suffix;
	    }else{
		push @result, $suffix;
	    }
	}
    }else{
	for ($self->_prefixes(@_)){
	    my ($prefix, @suffix) = @$_;
	    push @result, @suffix ? $prefix.$self->_trie_regex(@suffix) : $prefix;
	}
    }

    my $result;

  RESULT:
    {
	@result == 1 and $result = $result[0] and last RESULT;
	my $q = '';
	# alteration check
	# we do linear seach here to preserve order.
	for (my $i = 0; $i < @result; $i++){
	    if ($result[$i] eq ''){
		 splice @result, $i, 1;
		 $q = '?';
		 last;
	    }
	}
	# if ($result[0] eq '') { $q = '?'; shift @result }
	# extract character class
	if ($self->{optim_cc}){
	    my @char; my  $charpos = -1;
	    for (my $i = 0; $i < @result; $i++){
		if ($self->{quotemeta}){
		    if (length($result[$i]) == 2){ 
			$charpos < 0 and $charpos = $i;
			push @char => splice(@result, $i, 1, "");
		    }
		}else{
		    if ($result[$i] =~ /^$self->{_cclass}$/){
			$charpos < 0 and $charpos = $i;
			push @char => splice(@result, $i, 1, "");
		    }
		}
	    }
	    if (@char){
		my $char = $self->_optim_cc(@char);
		splice @result, $charpos, 0, $char;
		@result = grep {$_} @result;
		if (@result == 1){
		    $result = "$result[0]$q" and last RESULT;
		}
	    }
	}
	my $joiner = '|' ;
	if ($self->{optim_cq} and @result == 1 and
	    ($self->{quotemeta} 
	     ? length($result[0]) ==
	     1 : $result[0] =~ /^$self->{_token}$/))
	{
	    $result = qq/$result[0]$q/;
	}else{
	    $result = 
		$self->{po} . join($joiner => @result) . $self->{pc} .  $q;
	}
    }
    $self->{debug} and 
	print STDERR '<'x $self->{_indent}, " ", $result, "\n";
    $self->{_x} || $self->{debug} and $self->{_indent}--;

    $result;
}

sub _optim_cc{
    my $self = shift;
    @_ or return undef;
    if ($self->{quotemeta}){
	return  @_ ? @_ > 1  ? "[".join("",@_)."]" : $_[0] : undef;
    }
    # check '.'
    for (@_){
	$_ eq '.' and return '.';
    }
    my @char = @_;
    my ($positive, $negative) = ('','');
    my ($npos, $nneg) = (0, 0);
    for (@char){
	if    (s/^\[\^(.*)\]$/$1/){
	    $negative .= $_; $nneg += 2; next;
	}
	if (s/^\[(.*)\]$/$1/){
	    $positive .= $_; $npos += 2; next;
	}else{
	    #$positive .= length($_) eq 1 ? qq/\Q$_/ : $_;
	    $positive .= $_ eq '-' ? '\-' : $_;
	    $npos++;
	}
    }
    $nneg > 1 and $negative = qq/[^$negative]/;
    $npos > 1 and $positive = qq/[$positive]/;
    return $negative 
	?  $positive ? "(?:$positive|$negative)" : $negative
	    :  $positive;
}

1;
__END__

# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Regexp::List - builds regular expressions out of a list of words

=head1 SYNOPSIS

  use Regexp::List;
  my $l  = Regexp::List->new;
  my $re = $l->list2re(qw/foobar fooxar foozap fooza/);
  # $re is now qr/foo(?:[bx]ar|zap?)/

=head1 ABSTRACT

This module offers C<list2re> method that turns a list of words
into an optimized regular expression which matches all words therein.

The optimized regular expression is much more efficient than a
simple-minded '|'-concatenation thereof.

=head1 DESCRIPTION

This module use Object-Oriented approach so you can use this module as a base
and tweak its features.  This module is a base class of
L<Regexp::Optimizer>.

=head2 EXPORT

Since this is an OO module there is no symbol exported.

=head1 METHODS

This module offers methods below;

=over

=item $l = Regexp::List->new(I<< key=>value, ... >>)

Constructor.  When arguments are fed in I<< key => value, >> manner, it
sets attributes.  See C<< $l->set >> for details

=item $re = $l->list2re(I<list of words ...>)

Does the job.  Takes a list of words and turn it into an optimal
regular expresson.  See L</IMPLEMENTATION> to find out how it is
achieved.  If you want to know the underlying black magic even
further, see the source.

=item $l->set(I<< key => value, ... >>)

Sets attributes.  There are many attributes supported but let me
mention just a few that you may be interested.

=over

=item lookahead

Whether you prepend a lookahead assertion or not.  Default value is 1.
This module is smart enough to omit the assertion when you don't need one.

  $re = $l->list2re(qw/1 2 3 infinity/);
  # qr/(?=[123i])(?:[123]|infinity)/
  $re = $l->set(lookahead=>0)->list2re(qw/1 2 3 infinity/);
  # qr/(?:[123]|infinity)/

=item quotemeta

Whether you quote metacharacters or not.  Default is 1.  If you really
need this feature try L<Regexp::Optimizer> instead.

  @list = qw/3 3.14 3.14159265358979/;
  $re = $l->list2re(@list);
  # qr/3(?:\.14(?:159265358979)?)?)/
  $re = $l->set(lookahead=>0)->list2re(@list);
  # qr/3(?:.14(?:159265358979)?)?)/
  # which does match 3.14 but also "11+3=14"

=item modifies

Currently it accepts 'i', 'm', 's', and 'x', the same as regular
expression modifiers.

  @list = qw/Perl perl BASIC basic/;
  $re = $l->list2re(@list);
  # qr/(?=[BPbp])(?:[Pp]erl|BASIC|basic)/
  $re = $l->set(modifiers => 'i')->list2re(@list);
  # qr/(?=[bp])(?:perl|basic)/i
  print $l->set(modifiers => 'x')->list2re(@list);
  # Try for yourself;  Isn't itcute ?

=back

=item $l->expand($re);

Utility methods to expand a regular expression.  Handy when you
want to check the complex regexes.

=item $l->unexpand($re);

Utility methods to unexpand a regular expression.

=back

=head1 IMPLEMENTATION

This module optimizes the regular expression as follows.  Let's see
what happens when qw/foobar fooxar foozap fooza/ is fed

=over

=item trie building (common prefix aggregation)

first the corresponding I<trie> structure is built

       +- bar
  foo -+- xar
       +- za -+- p
              +- ''

=item common suffix aggregation

it checks if any leaf node can be optimized for common suffix.  In
this case we can do so to "bar" and "xar".

       +- b -+-ar
  foo -+- x -+
       +- za -+- p
              +- ''

=item character class conversion

If a branch contains more than two single characters, it turns it into
a character class.

  foo -+- [bx] --- ar
       +- za -+-p
              +- ''

=item empty leaf to C<?>

Empty leaf is converted to a '?' quantifier

  foo -+- [bx] --- ar
       +- za -+-p?


=item join all leafs into a group

And the final result is reached.

  foo(?:[bx]ar|zap?)

=back 

=head1 BENCHMARKS

This module is faily robust.  You can practically use this module to
find a regular expression that matches all words in a dictionary.
Here is a result by on perl 5.8.0, FreeBSD 4-Stable, Pentium III 800
Mhz with 512 MB RAM.

 # Sat May 31 09:11:06 2003 (     0.000000 s) Reading /usr/share/dict/words
 # Sat May 31 09:11:07 2003 (     0.847797 s) 235881 lines read.
 # Sat May 31 09:11:07 2003 (     0.000000 s) Making regexp.
 # Sat May 31 09:13:09 2003 (   121.596928 s) Done.
 # Sat May 31 09:13:09 2003 (     0.000000 s) Saving to t/words.rx
 # Sat May 31 09:13:09 2003 (     0.000000 s) Reading t/words.rx
 # Sat May 31 09:13:13 2003 (     3.679176 s) Done.
 # Sat May 31 09:13:13 2003 (     0.000000 s) Opening /usr/share/dict/words for comparison.
 # Sat May 31 09:13:13 2003 (     0.255222 s) /usr/share/dict/words:235881 lines found.
 # Sat May 31 09:13:13 2003 (     0.000000 s) Showtime!
 # 235881/235881
 # Sat May 31 10:44:17 2003 (  5464.370409 s) Done.
 # Sat May 31 10:44:17 2003 (  5464.370624 s) 43.167 matches/s

The result of optimization is obvious as the number of alteration
increases.  Here is a result of a benchmark which matches randomly
picked words against C</usr/share/dict/words>.

  ====        2 words
          Rate naive optim
  naive 1.79/s    --  -28%
  optim 2.49/s   39%    --

  ====      256 words
        s/iter naive optim
  naive   31.7    --  -81%
  optim   5.95  433%    --

=head1 SEE ALSO

L<Regexp::Optimizer> -- uses this module as its base

C<eg/> directory in this package contains example scripts.

=over

=item Perl standard documents

L<perltodo>, L<perlre>

=item CPAN Modules

L<Regexp::Presuf>, L<Text::Trie>

=item Books

Mastering Regular Expressions  L<http://www.oreilly.com/catalog/regex2/>

=back

=head1 AUTHOR

Dan Kogai <dankogai@dan.co.jp>

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Dan Kogai, All Rights Reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut

# $Id: Makefile.PL,v 0.2 2004/05/08 05:55:35 dankogai Exp $

use 5.006;
use strict;
use ExtUtils::MakeMaker;
use File::Spec;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

my $ModName = 'Regexp::Optimizer';
my $ModPath = File::Spec->catfile('lib', split(/::/ => $ModName)) . '.pm';

WriteMakefile(
    'NAME'		=> $ModName,
    'VERSION_FROM'	=> $ModPath, # finds $VERSION
    'PREREQ_PM'		=> {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM => $ModPath, # retrieve abstract from module
       AUTHOR     => 'Dan Kogai <dankogai@dan.co.jp>') : ()),
);

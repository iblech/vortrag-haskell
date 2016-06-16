#!/usr/bin/perl -i -p

use warnings;
use strict;

s/::/\\ensuremath{::}/g;
s/=\\PYGdefaultZgt{}|=>/\\ensuremath{\\Rightarrow}/g;
s/\\PYGdefaultZhy{}\\PYGdefaultZgt{}|->/\\ensuremath{\\rightarrow}/g;
s/<-/\\ensuremath{\\leftarrow}/g;
s/>=/\\ensuremath{\\geq}/g;
s/<=/\\ensuremath{\\leq}/g;
s/>>/\\ensuremath{\\gg}/g;

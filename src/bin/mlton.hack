#!/bin/sh

mlton -const 'Exn.keepHistory true' -default-ann 'allowFFI true' $*

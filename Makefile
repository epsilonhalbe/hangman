all :
	ghc --make -Wall Hangman.hs

clean :
	rm -f Hangman tHangman *.hi *.o report.html

test :
	ghc --make -Wall tHangman.hs
	./tHangman

prove :
# cpan App::Prove
	prove --exec make test


provehtml :
# cpan App::Prove::Plugin::HTML
	prove -P HTML=outfile:report.html --exec make test

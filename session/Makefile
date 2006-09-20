all: top.cgi login.cgi logout.cgi content.cgi upload.cgi

clean:
	rm -f *.hi *.o *.cgi

top.cgi: Top.hs Session.hs DateTime.hs Util.hs
	ghc --make -o $@ $<

login.cgi: Login.hs Session.hs
	ghc --make -o $@ $<

logout.cgi: Logout.hs Session.hs
	ghc --make -o $@ $<

content.cgi: Content.hs Session.hs
	ghc --make -o $@ $<

upload.cgi: Upload.hs DateTime.hs Session.hs
	ghc --make -o $@ $<

%.cgi: %hs
	ghc --make -o $@ $<

%.o %.hi: %.hs
	ghc -c $<

%.o %.hi: %.lhs
	ghc -c $<

# DO NOT DELETE: Beginning of Haskell dependencies
./SHA1.o : ./SHA1.lhs
User.o : User.hs
Top.o : Top.hs
Top.o : User.hi
Logout.o : Logout.hs
Login.o : Login.hs
Login.o : ./SHA1.hi
# DO NOT DELETE: End of Haskell dependencies

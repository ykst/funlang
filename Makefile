SRCDIR=src
BINDIR=bin
TARGET=$(BINDIR)/fun
TESTDIR=test

SRCS= $(SRCDIR)/Main.hs 
SRCS+=$(SRCDIR)/Interpretor.hs 
SRCS+=$(SRCDIR)/AST.hs 
SRCS+=$(SRCDIR)/Parser.hs 
SRCS+=$(SRCDIR)/Env.hs 

OBJS= $(SRCS:%.hs=%.hi) 
OBJS+=$(SRCS:%.hs=%.o)


.PHONY: clean test

$(TARGET): $(SRCS)
	ghc --make -o $@ $^

clean:
	$(RM) $(OBJS) $(TARGET)

test: $(TARGET)
	cd $(TESTDIR) && ./test_all.sh $(PWD)/$(TARGET)

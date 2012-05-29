SRCDIR=src
BINDIR=bin
TARGET=$(BINDIR)/fun

SRCS= $(SRCDIR)/Main.hs 
SRCS+=$(SRCDIR)/Interpretor.hs 
SRCS+=$(SRCDIR)/AST.hs 
SRCS+=$(SRCDIR)/Parser.hs 

OBJS= $(SRCS:%.hs=%.hi) 
OBJS+=$(SRCS:%.hs=%.o)

$(TARGET): $(SRCS)
	ghc --make -o $@ $^

clean:
	$(RM) $(OBJS) $(TARGET)

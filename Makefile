#
# This Makefile was automatically generated by Code::Blocks IDE.
#

SRCS_f95d1 = \
result.f95 \
main.f95 

OBJS_f95d1 = \
result.o \
main.o 

SRC_DIR_f95d1 = 
OBJS_DIR = obj/Debug/
EXE_DIR = bin/Debug/

EXE = Numerico.exe
FC = mingw32-gfortran.exe
IDIR = 
CFLAGS = -Wall -Wall -g -Wall -g  -J$(OBJS_DIR) $(IDIR)
LFLAGS = 
LIBS = 

VPATH = $(SRC_DIR_f95d1):$(OBJS_DIR)
OBJS = $(addprefix $(OBJS_DIR), $(OBJS_f95d1))

all : $(EXE)

$(EXE) : $(OBJS_f95d1)
	@mkdir -p $(EXE_DIR)
	$(FC) -o $(EXE_DIR)$(EXE) $(OBJS) $(LFLAGS) $(LIBS)

$(OBJS_f95d1):
	@mkdir -p $(OBJS_DIR)
	$(FC) $(CFLAGS) -c $(SRC_DIR_f95d1)$(@:.o=.f95) -o $(OBJS_DIR)$@

clean :
	rm -f $(OBJS_DIR)*.*
	rm -f $(EXE_DIR)$(EXE)

# Dependencies of files
result.o: \
    result.f95
main.o: \
    main.f95


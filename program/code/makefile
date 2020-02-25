FF = gfortran
FFLAGS = -g -Wall
 LIBS = -llapack

 all: main clean

 main: main.o equilibrium.o newtons_method.o numerical_jacobian.o calibrate.o globals.o csv_file.o
	 $(FF) -o main main.o equilibrium.o newtons_method.o numerical_jacobian.o calibrate.o globals.o csv_file.o $(FFLAGS) $(LIBS)

 main.o: main.f95 globals.o equilibrium.o newtons_method.o numerical_jacobian.o calibrate.o csv_file.o
	 $(FF) -c $< $(FFLAGS)

equilibrium.o: equilibrium.f95 calibrate.o newtons_method.o numerical_jacobian.o globals.o csv_file.o
	$(FF) -c $< $(FFLAGS)

calibrate.o: calibrate.f95 globals.o csv_file.o
	$(FF) -c $< $(FFLAGS)

newtons_method.o: newtons_method.f95 numerical_jacobian.o globals.o
	$(FF) -c $< $(FFLAGS)

numerical_jacobian.o: numerical_jacobian.f95 globals.o
	$(FF) -c $< $(FFLAGS)

globals.o: globals.f95 csv_file.o
	$(FF) -c $< $(FFLAGS)

csv_file.o: csv_file.f90
	$(FF) -c $< $(FFLAGS)

clean:
	rm *.o
	rm *.mod


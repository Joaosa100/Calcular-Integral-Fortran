program trapezoidal
    
implicit none
real,external::f
real :: a,b,h,p,integration,summ,y,x
integer::n,i,teste

print*, "coloque o limite mais baixo da integracao:"
read*, a

print*, "coloque o limite mais alto da integracao:"
read*, b

print*, "Coloque o numero de intervalos:"
read*, n

h=(b-a)/n
p=(h/2.)*(f(a)+f(b))
summ=0
do i=1,n-1
    summ=summ+h*f(a+i*h)
enddo
integration=p+summ

print*, "O valor da integration e': ", integration !basta colocar integration

open(1,file="funcao.dat",status="new")

do teste = -3, 3, 1
  y=1+teste**2
  write(1,*) teste,y
enddo
close(1)

! Chama o GNUplot para plotar a função

call system("gnuplot -p -c plot.gp") !vai criar um arquivo chamado funcao.png lendo o arquivo plot.gp
call system("rm funcao.dat")

end

real function f(x)
f = sqrt(1 + x**2) !ou f = (1+x**2)**0.5
end
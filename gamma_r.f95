! 14/03/2020
! gamma.f95
! Elser Lopez (eladolfos@gmail.com)
!-----------------------------------
!    Programa Funcion Gamma:
!    Este programa calcula la funcion gamma de un numero Z complejo, con Re(z)>0
!    utilizando la definicion de recursividad de Weierstrass
!-----------------------------------
!
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Xubuntu Linux) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran  -Wall -pedantic -std=f95 -o gamma_r.e gamma_r.f95
!    Copyright (C) 2020
!    E.A. López
!    eladolfos@gmail.com
!
!    This program is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see
!    <http://www.gnu.org/licenses/>.
!
!
!   1. INICIO
    program test_cmplx
    implicit none
!   2. Definir: variables del problema y auxiliares
        integer(8) :: n
        real(8) :: x
        real(8) :: y
        complex(8) :: z, result
!   3. Definir: la funcion Gamma_r
        complex(8) :: Gamma_r
!   4. Leer: el numero complejo z. Parar si la parte real es <0
        OPEN(unit=1,File='data.num', status='old')
        READ(1,*) x !componente real x del numero complejo
        READ(1,*) y !componente imaginaria y del numero complejo
        CLOSE(1)
        If((x-int(x))==0)THEN !el numero es un entero
            !hay que comprobar que sea positivo
            IF(x<=0) THEN !esto se da si es negativo, hay que detener el programa
                WRITE(*,*) 'Se ha ingresado un numero complejo z=0,-1,-2...'
                WRITE(*,*) 'El programa ha sido detenido'
            stop
            END IF
        END IF
        z = cmplx(x, y,8) !convertiendo a un numero complejo
        n=20 !numero para el productorio infinito
        result=Gamma_r(z,n)
!   5. Escribir: en pantalla el valor de la funcion gamma
        WRITE(*,*) REAL(result), AIMAG(result)
!   6. FIN
end program test_cmplx

!FUNCION RECURSIVA DE Weierstrass

recursive function Gamma_r(z, n) result(r)
IMPLICIT NONE
    complex(8), intent(in):: z
    complex(8) :: r
    integer(8) :: n
    complex(8) :: Gamma_Euler
        Gamma_r(z,n)=Gamma_Euler(z,n)
        IF (REAL(z)==1 .and. AIMAG(z)==0) THEN
            r=Gamma_Euler(z,n)
        ELSE    
            r=Gamma_r(z-1,n)*(z-1)
        END IF
end function Gamma_r

!FUNCION GAMMA USANDO EL PRODUCTO INFINITO DE EULER
FUNCTION Gamma_Euler(z,n)
implicit none
    complex(8) :: z, Gamma_Euler, factorialZ
    integer(8) :: n, factorial
    Gamma_Euler=factorial(n)*n**z/factorialZ(z,n)
END FUNCTION Gamma_Euler
!FUNCION PARA EL DENOMIDADOR DEL PRODUCTO INFINITO DE EULER 
FUNCTION factorialZ(z,n)
implicit none
    integer(8) :: n
    complex(8) :: z, factorialZ
    integer(8) :: i
    factorialZ=z
    DO i=1, n
        factorialZ=(z+i)*factorialZ
    END DO
END FUNCTION factorialZ

!FUNCION FACTORIAL
FUNCTION factorial(x)
implicit none
    integer(8) :: factorial
    integer(8) :: x
    integer(8) :: i
    factorial=1
    DO i=1, x
        factorial=factorial*i
    END DO
RETURN
END FUNCTION factorial
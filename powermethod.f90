program power_method_no_reshape
    implicit none

    real :: A(3,3)
    real :: X(3) = [1.0, 1.0, 1.0]
    real :: Y(3)
    real :: lambda = 0.0, prelemda = 0.0
    integer :: step

    A(1, :) = [ -4.0, 14.0,  0.0 ]
    A(2, :) = [ -5.0, 13.0,  0.0 ]
    A(3, :) = [ -1.0,  0.0,  2.0 ]

    ! সহজ এবং পরিষ্কার হেডার
    print *, "Step    Lambda         Eigenvector (X)"

    do step = 1, 20
        Y = matmul(A, X)
        lambda = maxval(abs(Y))
        X = Y / lambda

        ! [ X(1), X(2), X(3) ] এই ফরমেটে প্রিন্ট হবে, দশমিকের পর ৪ ঘর থাকবে
        print '(I4, F7.4, A4, F7.4, A2, F7.4, A2, F7.4, A2)', &
            step, lambda, "   [", X(1), ", ", X(2), ", ", X(3), " ]"

        if (abs(lambda - prelemda) < 0.0001) exit
        prelemda = lambda
    end do
end program power_method_no_reshape

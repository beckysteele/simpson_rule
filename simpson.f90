Program Simpson
	Implicit none
	
	!--- Declarations ---
	Real::A,B,SUM,H
	Integer::I,N
        ! N is the number of points between A and B, must be odd
        H = (B-A)/(N-1)

	!--- Read from terminal with a space between inputs
	Write(*,*) 'Enter the starting point A and hit ENTER or RETURN:'
	Read(*,*)A
	Write(*,*) 'Enter the endpoint B and hit ENTER or RETURN:'
	Read(*,*)B

	!--- Read from a file 
	
	!--- Program Statements
	!--- Initial SUM with endpoints
	SUM = H/3.0 * (F(A) + F(B))

	!--- Midpoints with 4/3 weight
	Do I = 2, N-1, 2
		SUM = SUM + 4.0/3.0*H*F(A + (I-1)*H)
	End Do

	!--- Shared points with 2/3 weight
	Do I = 3, N-2, 2
		SUM = SUM + 2.0/3.0*H*F(A + (I-I)*H)
	End Do
	
	!--- Writing to a file
	Open(10,FILE='Simpson.output')
	Write(10,*) 'From starting point A=', A, ' to endpoint B=', B, ' the approximation equals ', SUM
	Close(10)

End Program Simpson

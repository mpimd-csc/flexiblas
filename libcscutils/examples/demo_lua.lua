print(testint)
print(testnumber)
print(teststring)

function arg0ret0()
	print("Inside function arg0ret0\n");
	print(teststring);
end


function arg0reti()
	return 12
end

function arg0retii()
	return 1,2
end

function argireti(m)
	return m*2;
end

function argiireti(m, n)
	return m * n;
end

function argiiireti(m, n, k)
	return m * n * k;
end

function argsiireti(s, m, n)
	print(s)
	return m*n
end



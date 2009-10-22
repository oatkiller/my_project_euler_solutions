# include <stdio.h>

int fi(int n)
{ 
	int result = n; 
	int i;
	for(i=2;i*i <= n;i++) 
	{ 
		if (n % i == 0) result -= result / i; 
		while (n % i == 0) n /= i; 
	} 
	if (n > 1) result -= result / n; 
	return result; 
}

int cototient (int n)
{
	return n - fi (n);
}

int main ()
{
	printf ("%i",cototient(28));
	printf ("\n");
}

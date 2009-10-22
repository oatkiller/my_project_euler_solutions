var factor = function (n) { 
	var i = 2, floor = Math.floor, sqrt = Math.sqrt, factors = [];
 	for (; i <= floor(sqrt(n)); i++) { 
		while (n % i == 0) { 
			factors.push(i);
		 	n = floor(n / i); 
	 	} 
 	} 
 	if (n > 1) factors.push(n);
	return factors;
}; 

var get_occurance_of_factors = function (factors) {

	var occurance = {};

	factors.forEach(function (n) {
		occurance[n] = n in occurance ? occurance[n] + 1 : 1;
	});

	return occurance;
};

var get_args_from_occurance = function (occurance) {
	
	var xs = [];

	for (var factor in occurance) {
		xs.push(occurance[factor]);
	}

	return xs;

};

var get_number_of_factors = function (n) {
	var factors = factor(n),
	occurance = get_occurance_of_factors(factors),
	args = get_args_from_occurance(occurance);

	return args.reduce(function (memo,n) {
		return memo * (n + 1);
	},1);

};

var get_triangle = function (n) {
	return n * (n + 1) / 2;
};


var number_of_factors = 1, i = 1;
while (number_of_factors < 500) {
	number_of_factors = get_number_of_factors(get_triangle(++i));
};

console.log(get_triangle(i));

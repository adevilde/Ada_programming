package memoire is
	
	type T_bit is mod 2;
	for T_bit'Size use 1;

	type T_byte is mod 2 ** 8;
	for T_byte'Size use 8;

	type T_bits is array (1..8) of T_bit;

	Function To_byte (bits : in  T_bits) return T_byte;
	
	Function To_bits (byte : in T_byte) return T_bits;

end memoire;

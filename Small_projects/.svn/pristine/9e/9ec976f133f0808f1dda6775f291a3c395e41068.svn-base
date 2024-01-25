package body memoire is


	Function To_byte (bits : in T_bits) return T_byte is
		byte : integer := 0;
	begin
		for i in 1..8 loop
			byte := byte + Integer'Val(bits(i)) * 2**(8-i);
		end loop;
		return T_byte(byte mod 2**8);
	end To_byte;



	Function To_bits (byte : in T_byte) return T_bits is
		i_octet : integer := Integer'Val(byte);
		bits : T_bits;
	begin
		for i in 0..7 loop
			bits(8-i) := T_bit(i_octet mod 2);
			i_octet := i_octet / 2;
		end loop;
		return bits;
	end To_bits;

end memoire;

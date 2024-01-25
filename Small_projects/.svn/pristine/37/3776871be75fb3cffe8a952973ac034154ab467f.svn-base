with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;


package body Fichier is


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


	Procedure Creer (file_name : in string) is
		file	: Ada.Streams.Stream_IO.File_Type		;
	begin
		Create (file, Out_file, file_name);
		Close (file);
	end Creer;


	Function Taille (file_name : in string) return integer is
		file	: Ada.Streams.Stream_IO.File_Type		;
		S	: Stream_Access					;
		byte	: T_byte					;
		c 	: integer				:= 0	;
	begin
		Open(file, In_File, file_name);
		S := Stream(File);
		while not End_Of_File(File) loop
			byte := T_byte'Input(S);
			c := c + 1;
		end loop;
		Close (File);
		return c;
	end Taille;


	Procedure Lire_bit (file_name : in string ; tab : out T_tab_bit) is
		file	: Ada.Streams.Stream_IO.File_Type		;
		S	: Stream_Access					;
		byte	: T_byte					;
		bits 	: T_bits 					;
		i 	: integer				:= 0	;
	begin
		Open(file, In_File, file_name);
		S := Stream(file);
		while not End_Of_File(File) loop
			byte := T_byte'Input(S);
			bits := To_bits(byte);
			for j in 1..8 loop
				tab(8*i + j) := bits(j);
			end loop;
			i := i + 1;
		end loop;
		Close (file);
	end Lire_bit;


	Procedure Lire_byte (file_name : in string ; tab : out T_tab_byte) is
		file	: Ada.Streams.Stream_IO.File_Type		;
		S	: Stream_Access					;
		byte	: T_byte					;
		i 	: integer				:= 1	;
	begin
		Open(file, In_File, file_name);
		S := Stream(file);
		while not End_Of_File(File) loop
			byte := T_byte'Input(S);
			tab(i) := byte;
			i := i + 1;
		end loop;
		Close (file);
	end Lire_byte;


	Procedure Ecrire (file_name : in string ; tab : in T_tab_bit) is
		file	: Ada.Streams.Stream_IO.File_Type		;
		S	: Stream_Access					;
		bits 	: T_bits 					;
	begin
		Create (file, Out_file, file_name);
		S := Stream (file);
		for i in 0..size - 1 loop
			for j in 1..8 loop
				bits(j) := tab(8*i + j);
			end loop;
			T_byte'Write(S, To_byte(bits));
		end loop;
		Close (file);
	end Ecrire;


	Procedure Ajouter (file_name : in string ; bits : in T_bits) is
		file	: Ada.Streams.Stream_IO.File_Type		;
		S	: Stream_Access					;
	begin
		Open(file, Out_File, file_name);
		S := Stream(file);
		T_byte'Write(S, To_byte(bits));
		Close (file);
	end Ajouter;
		

end Fichier;


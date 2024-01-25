with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;
with decode;			use decode;
with Ada.Streams.Stream_IO;	use Ada.Streams.Stream_IO;
with types;			use types;

procedure test_decode is

	procedure tester_Lire is
	tab : T_decode;
	code : T_code;
	tab_code : T_tab_code;
	lng_decode : integer;
	begin
		Lire("test.txt", code, tab_code, lng_decode);
		if tab /= decode or tab_code'last - tab_code'first /= 2 then
			Put("Erreur");
		end if;

		if tab_code(1) /= 1 or tab_code(2) /= 0 then 
			Put("Erreur");
		end if;
		
		if lng_decode /= 10 then
			Put("Erreur");
		end if;	
	end tester_Lire;

	procedure tester_Ecrire is 
	type T_tab is new T_decode(lng_decode => 100);
	tab : T_tab;	
	File_Name : String :=  "test_ecrire.txt";
	File      : Ada.Streams.Stream_IO.File_Type;
	S         : Stream_Access;
	int : integer;
	c :integer := 1;
	begin
		for i in 1..100 loop
			tab(i) := i;
		end loop;
		Ecire("test_ecrire.txt", tab);
		Open(File, In_File, File_Name);
		S := Stream(File);
		while not End_Of_File(File) loop
			int := integer'Input(S);
			if int /= tab(c) then
				Put("Erreur");
			end if;
			c := c + 1;
		end loop;

	end tester_Ecrire;

begin 

	tester_Lire;
	tester_Ecrire;	

end test_decode;

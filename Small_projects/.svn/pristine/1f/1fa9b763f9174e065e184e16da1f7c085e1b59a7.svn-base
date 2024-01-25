with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;
with code;			use code;
with Ada.Streams.Stream_IO;	use Ada.Streams.Stream_IO;
with types;			use types;

procedure test_code is
        
        procedure tester_Lire is
        decode : T_decode;
        lng_decode : Integer;

        begin 
                Lire("test.txt", decode, lng_decode);
                if lng_decode /= 10 then 
                        Put("Erreur");
                end if;

        end tester_Lire;


        procedure tester_Premiere_branche is
        decode : T_decode;
        tab_branche : T_tab_branche;
        lng_decode : Integer;

        begin 
                Premiere_branche(decode, tab_branche, lng_decode);
                if lng_decode <= 0 then
                        Put("Erreur");
                end if;

                if tab_branche(1).all.branche_d = Null and tab_branche(1).all.branche_g = Null then
                        Put("Erreur");
                end if;
        end tester_Premiere_branche;


        procedure tester_ArbreH is
        tab_branche : T_tab_branche;
        lng_decode : Integer;

        begin
                ArbreH (tab_branche, lng_decode);
                
        end tester_ArbreH;


        procedure tester_Tableau_code is
        arbre : T_arbre;
        tab_code : T_tab_code;

        begin
                Tableau_code (arbre, tab_code);

        end tester_Tableau_code;


        procedure tester_Traduire is 
        code : T_code;

        begin
                Traduire ("test.txt", code);

        end tester_Traduire;

        procedure tester_Ecrire is 
	code : T_code;
        tab_code : T_tab_code;
        File_Name : String := "test_ecrire.txt";
	File      : Ada.Streams.Stream_IO.File_Type;
	S         : Stream_Access;
	int : integer;
	c :integer := 1;
	begin
		S := Stream (File);
	        for I in 0..128 loop
		        T_code'Write(S, T_code(I));
        	end loop;

                Ecire("test_ecrire.txt", tab_code);
		Open(File, In_File, File_Name);
		S := Stream(File);
		while not End_Of_File(File) loop
			int := integer'Input(S);
			if int /= tab_code(c) then
				Put("Erreur");
			end if;
			c := c + 1;
		end loop;

	end tester_Ecrire;


begin
        tester_Lire;
        tester_Premiere_branche;
        tester_ArbreH;
        tester_Tableau_code;
        tester_Traduire;
        tester_Ecrire;

end test_code;

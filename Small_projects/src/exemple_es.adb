with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure Exemple_ES is
	type T_Octet is mod 2;	-- sur 8 bits
	for T_Octet'Size use 1;

	File_Name : String :=  "exemple_fichier.out";
	File      : Ada.Streams.Stream_IO.File_Type;	-- car il y a aussi Ada.Text_IO.File_Type
	S         : Stream_Access;
	Octet     : T_Octet;

begin
	-- Ã‰crire les premiers octets dans un fichier
	-- ------------------------------------------
	--   CrÃ©er un fichier en Ã©criture (Ã©crasement si existant)
	Create (File, Out_File, File_Name);

	--   Ã‰crire dans le fichier via un Stream
	--   (on pourrait Ã©crire des donnÃ©es de type diffÃ©rents)
    S := Stream (File);
    tab = (1, 0, 1, 1, 0, 1)
	for I in 1..6 loop
		T_Octet'Write(S, tab(I));
	end loop;

	--   Fermer le fichier
	Close (File);


	-- Lire le contenu du fichier
	-- --------------------------
	--   Ouvrir le fichier en lecture
	Open(File, In_File, File_Name);

	--   Lire, vÃ©rifier et afficher de temps en temps le contenu
	--   Attention, il faut lire les donnÃ©es dans le mÃªme ordre qu'elles ont Ã©tÃ© Ã©crite.
	--   Ici, le problÃ¨me ne se pose pas car il n'y a que des octets.
	S := Stream(File);
	while not End_Of_File(File) loop
		Octet := T_Octet'Input(S);
		Put("Octet = " & T_Octet'Image(Octet));
		Put(" '" & Character'Val(Octet) & "'");
		New_Line;
	end loop;

	--   Fermer le fichier
	Close (File);

end Exemple_ES;

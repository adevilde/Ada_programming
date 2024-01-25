with Ada.Text_IO; use Ada.Text_IO;
with arbre; use arbre;
with decode;
with memoire; use memoire;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with affichage; use affichage;
with Ada.Command_Line; use Ada.Command_Line;

procedure decompresser is
	
	file_name_in : Unbounded_String ;
	file_name_out : Unbounded_String ;

	nbu_byte : integer;
	lng_parcours_infixe : integer;
	byte_ff : T_byte;

	package decode_i is new decode(lng_parcours_infixe => 10, lng_tab_feuilles => 10);

	Option_inconnue : exception;
	Trop_arguments : exception;
	Pas_arguments : exception;
	
	Bavard : Boolean := False;


procedure decompresser(file_name_in : in Unbounded_String ; Bavard : in boolean ; nbu_byte : in integer ; lng_parcours_infixe : in integer ; byte_ff : in T_byte) is
	
	package decode_tailles is new decode(lng_parcours_infixe 	=> lng_parcours_infixe,
					     lng_tab_feuilles 		=> nbu_byte	      );
	use decode_tailles;

	arbre : T_arbre;
	parcours_infixe : T_parcours_infixe;
	tab_feuilles : T_tab_feuilles;
	i : integer := 1;
	f : integer := 1;
	rff : boolean := True;


	Est_Gauche : constant boolean := False;
	indent : constant Unbounded_String := To_Unbounded_String("");

begin
	Lire(To_String(file_name_in), parcours_infixe, tab_feuilles);
	ReconstruireH (parcours_infixe, tab_feuilles, byte_ff, arbre, i, f, rff);

	file_name_out := To_Unbounded_String(Slice(file_name_in, 1, Ada.Strings.Unbounded.Length(file_name_in) - 4));
	Decoder(To_String(file_name_in), To_String(file_name_out), arbre);
	if Bavard then 
		Put_Line("Arbre de Huffman : ");
		New_Line;
		Afficher_arbreH(arbre, indent, Est_Gauche);
		New_Line;
	end if;
end decompresser;



begin
	case Argument_Count is 

		when 0 => raise Pas_arguments;

		when 1 => null;

 		when 2 => 
			if Argument(1) = "-b" or Argument(1) = "--bavard" then
 				Bavard := True;
 			else
 				raise Option_inconnue;
			end if;

 		when others => raise Trop_arguments;

 	end case;

	file_name_in := To_Unbounded_String(Argument (Argument_Count));
	decode_i.Tailles(To_String(file_name_in), nbu_byte, lng_parcours_infixe, byte_ff);
	decompresser(file_name_in, Bavard, nbu_byte, lng_parcours_infixe, byte_ff);

	exception
		when Option_inconnue => Put_Line("L'option est inconnue");
		when Trop_arguments => Put_Line("Il y a trop d'arguments. Il faut en mettre au plus 2.");
		when Name_Error => Put_Line("Ce fichier est inexistant");
		when Pas_arguments => Put_Line("Usage : ./compresser nom_fichier");
end decompresser;

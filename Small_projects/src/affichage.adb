with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with arbre; use arbre;
with memoire; use memoire;
with fichier;
with code;

package body affichage is

	package fichier_1 is new fichier(size => 1);
	length : integer := fichier_1.Taille("exemple_huff.txt");

	package fichier_length is new fichier(size => length);
	use fichier_length;


	

	procedure Afficher_arbreH (Arbre_huff : in out T_arbre;
							indent : in Unbounded_String ;
							Est_Gauche : in Boolean) is

		Branche_droite : T_arbre;
		Branche_gauche : T_arbre;
		new_indent : Unbounded_String := indent;

	begin

		if not Est_Vide(Arbre_huff) then
			if indent /= "" then
				Put(To_String(indent));
				Put("\--");
				if Est_Gauche then
					Put("0--");
				else
					Put("1--");
				end if;
			end if;

			Put("(");
			Put(La_freq(Arbre_huff),1);
			Put(")");

			if Terminal(Arbre_huff) then
				Put(" '");
				Put(Character'Val(Le_byte(Arbre_huff)));
				Put("'");
			end if;

			New_Line;

			if indent = "" then
				Append(new_indent, " ");
			elsif Est_Gauche then
				Append(new_indent, "|     ");
			else
				Append(new_indent, "      ");
			end if;

			if not Terminal(Arbre_huff) then
				Branche_gauche := Branche_g(Arbre_huff);
				Afficher_arbreH(Branche_gauche, new_indent, True);

				Branche_droite := Branche_d(Arbre_huff);
				Afficher_arbreH(Branche_droite, new_indent, False);
			end if;
		else
			New_Line;
		end if;

	end Afficher_arbreH;

end affichage;


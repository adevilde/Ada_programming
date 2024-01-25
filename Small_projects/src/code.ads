with Arbre; use arbre;
with memoire; use memoire;
with fichier;

generic 
	file_size : integer;
	nbu_byte : integer;
	

package code is
	
	package fichier_taille is new fichier(size => file_size);

	type T_trad is limited private;
	type T_tab_code is limited private;
	type T_tab_branche is limited private;
	type T_pi is limited private;
	type T_bpi is limited private;
	type T_code is limited private;
	--Type T_code is array (1..256) of T_bit;
	
	
	-- Déterminer le nombre de caractères uniques dans le fichier
	function nb_caractere_unique (file_name : in String) return Integer;	
	
	
	-- Initialiser un tableau d'arbre (correspondant à une branche)
	procedure Initialiser_tab_branche(tab_branche : out T_tab_branche);
	
	
	-- Construction de la premier branche de l'arbre correspondant au tableau des fréquences
	-- de chaque caractère présent dans le fichier texte 
	procedure Premiere_branche (file_name : in string ; 
							 tab_branche : in out T_tab_branche);
	
	
	-- Calcul de l'indice et de la valeur du minimum de fréquence dans un tableau d'arbre.
	procedure Mins(tab_branche : in T_tab_branche ; 
				min1 : out T_arbre ; 
				imin1 : out integer ; 
				min2 : out T_arbre ; 
				imin2 : out integer);
	
	
	-- Construction de l'arbre de Huffman 
	procedure ArbreH (tab_branche : in out T_tab_branche ; 
					arbre : in out T_arbre);

	
	-- Initialiser la table de codage de Huffman
	procedure Initialiser_tab_code (tab_code : in out T_tab_code);

	
	-- Constuire la table de codage de Huffman suivant un parcours infixe de l'arbre
	procedure Tableau_code (arbre : in T_arbre ; 
						 tab_code : out T_tab_code ; 
						 c_code : in T_code ; 
						 c_code_l : in integer ; 
						 pi : out T_pi ; 
						 bpi : out T_bpi ; 
						 ipi : in out integer ; 
						 ibpi : in out integer);

	
	-- Traduire le fichier pris en entrée à l'aide de la table de codage de Huffman dans un 
	-- fichier compressé de sortie en format .hff
	procedure Traduire (file_name_in : in string ; 
					 file_name_out : in string ; 
					 tab_code : in T_tab_code ; 
					 pi : in T_pi ; 
					 bpi : in T_bpi);


	-- Accéder au code de Huffman associé à un octet
	procedure Acces_tab_code (tab_code : in T_tab_code ; 
						   i : in integer ; 
						   val : out T_code ; 
						   length : out integer);


	procedure Acces_tab_branche (tab_branche : in T_tab_branche ; i : in integer ; noeud : out T_arbre);


	function Acces_code (code : in T_code ; i : in integer) return integer;


	function Acces_pi (pi : in T_pi ; i : in integer) return integer;


	function Acces_bpi (bpi : in T_bpi ; i : in integer) return integer;

	
	-- Supprimer tous les éléments d'un tableau d'arbre 
	procedure Free (arbre : in out T_arbre ; 
				 tab_branche : in out T_tab_branche);
	
	
private
	
	Type T_code is array (1..256) of T_bit;
	Type T_trad is record
		length : integer;
		value : T_code;
	end record;
	Type T_tab_code is array (0..256) of T_trad;
	
	Type T_tab_branche is array (0..256) of T_arbre;
	type T_pi is array (1..(nbu_byte) * 2) of T_bit;
	
	type T_bpi is array (0..nbu_byte) of T_byte;
	
end code;


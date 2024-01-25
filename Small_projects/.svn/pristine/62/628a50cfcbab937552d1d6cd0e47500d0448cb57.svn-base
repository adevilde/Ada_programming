with fichier;
with memoire; use memoire;

package Arbre is

	package fichier_0 is new fichier(size => 1);
	
	type T_arbre is private;

	-- Initialiser un arbre.  L'arbre est vide.
	procedure Initialiser(arbre : out T_arbre);
            
	-- Remplir le noeud d'un arbre
	procedure Remplire (arbre : out T_arbre);


	-- Est-ce qu'un arbre est vide ?
	function Est_Vide (arbre : T_arbre) return Boolean;


	-- Est-ce qu'un arbre est terminal ?
	function Terminal (arbre : T_arbre) return Boolean;


	-- Obtenir le nombre d'éléments d'un arbre. 
	function Taille (arbre : in T_arbre) return Integer;


	-- Affecter des données au premier noeud d'un arbre.
	procedure Affecter (arbre 	: 	in out 	T_arbre 			; 
			    byte 	: 	in 	T_byte 		:= T_byte(0) 	; 
			    freq 	: 	in 	integer 			;
			    branche_g 	: 	in 	T_arbre 			;
			    branche_d 	: 	in 	T_arbre 			);

	
	-- Transformer un arbre en fin de fichier
	procedure Devient_ff (arbre : out T_arbre);

	
	-- Est-ce qu'un arbre est une fin de fichier ?
	function Est_ff (arbre : in T_arbre) return Boolean;


	--Retourne la branche gauche de l'arbre.
	function Branche_g (arbre : in out T_arbre) return T_arbre;


	--Retourne la branche droite de l'arbre.
	function Branche_d (arbre : in out T_arbre) return T_arbre;


	-- Savoir si un octet est présent dans un arbre.
	function Byte_present (arbre : in T_arbre ; byte : in T_byte) return Boolean;


	-- Obtenir la fréquence associée à un noeud dans l'arbre.
	-- Exception : octet_Absente_Exception si octet n'est pas utilisé dans l'arbre
	function La_freq (arbre : in T_arbre) return integer;

	-- Obtenir l'octet associé à une feuille dans l'arbre
	function Le_byte (arbre : in T_arbre) return T_byte;


	-- Supprimer tous les éléments d'un arbre.
	procedure Vider (arbre : in out T_arbre);


private
        
        type T_noeud;

        type T_arbre is access T_noeud;

        type T_noeud is
                record
			freq : Integer;        
			byte : T_byte;
			branche_d : T_arbre;
			branche_g : T_arbre;
			ff : Boolean;
                end record;

end Arbre;

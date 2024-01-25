with ada.text_io; use ada.text_io;
with ada.command_line; use ada.command_line;
with ada.numerics; use ada.numerics;
with ada.calendar; use ada.calendar;
    
-- I/O sur tous les caractères Ascii.
with sequential_io;
     
procedure Projet is

--/*****************************************************************    
--/* Ce programme compresse et décompresse des fichiers de données *
--/* selon la méthode de Huffman.                                  *
--/*****************************************************************
      
package Fichier_io is new sequential_io(character);
  
--/------------------------------------------------/
--/ Déclarations concernant la liste chainée.
--/------------------------------------------------/
     
type Cellule;
   
type Pointeur is access Cellule;

-- Définition d'une cellule qui contiendra le caractère
-- et le nombre de fois qu'il apparaît dans le fichier.    
type Cellule is record
	Code : integer;
    Frequence : natural;
    Suivant : Pointeur;
    FilsGauche : Pointeur;
    FilsDroit : Pointeur;
end record;
    
type Vecteur is array (0..256) of integer;

Tableau : Vecteur := (others => 0);
Liste : Pointeur := null;

--/------------------------------------------------/
--/ Déclarations concernant l'arbre
--/------------------------------------------------/

type Bit is array (1..257) of natural range 0..1;

type Objet is record
    Longueur : natural := 0;
	Binaire : Bit := (others => 0);
end record;

type InfoCaractere is array (0..256) of Objet;

Arbre : Pointeur;
Tampon : Bit;
CaractereCode : InfoCaractere;
Hauteur : natural := 1;

--/------------------------------------------------/
--/ Déclaration concernant les fichiers
--/------------------------------------------------/

Source, Destination : Fichier_io.file_type;
LongueurNomSource, LongueurNomDestination : natural;
NomFichierSource : string(1..500);
NomFichierDestination : string(1..500);

--/-----------------------------------------------/
--/ Divers
--/-----------------------------------------------/

MAUVAIS_FORMAT : exception;
TimeDebut, TimeFin : Time;
NombreCaractereSource, NombreCaractereDestination : Float := 0.0;

--/------------------------------------------------/ 
--/ Déclaration des procédures et des fonctions
--/------------------------------------------------/

Procedure AfficheAide is 
        
--/ Cette procédure affiche l'aide à l'écran.
        
begin    
        
	new_line;
    put_line("Projet version 1.0");
    put_line("Mai 1999 (c) Jérôme Serré");
    put_line("CNAM - TP VARI.");
    new_line;
    put_line("[Projet <commande> <source> <destination>]");
    new_line;
    put_line("<commande>");
    put_line(" d : décompresse le fichier source dans le répertoire destination.");
    put_line(" c : compresse le fichier source dans le fichier destination.");
    put_line(" ? : Affiche l'aide à l'écran.");
    new_line;
    put_line("Exemple:");
    put_line("Projet c tartuf.txt e:\tartuf.huf");
    put_line("Projet d tartuf.huf c:\theatre\moliere\");    
    
end AfficheAide;					 
    
procedure ChargementCaractere(Tableau : out Vecteur) is
        
--/ Tant que la fin de fichier n'est pas atteinte on lit caractère par 
--/ caractère tout le fichier.
--/ La fréquence de chaque caractère est rangée dans un tableau d'indice variant de 0 à 256.
--/ On fait correspondre le rang (Table des caracteres Ascii) du caractère à l'indice.

--/ EXEMPLE:
--/ S'il y a 8 caractères "P" de rang 80 dans le texte, on aura Tableau(80) = 8.
        
Caractere : character; 
        
begin
        
	Fichier_io.open(Source, Fichier_io.in_file, NomFichierSource(1..LongueurNomSource));    
        
    while not Fichier_io.end_of_file(Source) loop
            
    	-- Lecture du fichier caractère par caractère.
        Fichier_io.read(Source, Caractere);
        NombreCaractereSource := NombreCaractereSource + 1.0;
            
        -- Mise à jour de la fréquence. 
		Tableau(character'pos(Caractere)) := Tableau(character'pos(Caractere)) + 1;
        
    end loop;
        
    -- Insertion du caractère fin de fichier.
    Tableau(256) := 1;		
        
    Fichier_io.close(Source);
        
end ChargementCaractere;

procedure CreationListe(Liste : in out Pointeur; Tableau : in Vecteur) is
        
--/ Cette procédure crée une liste chainée à partir du tableau de caractère.
        
begin
        
	for i in 0..256 loop
        
        if (Tableau(i) /= 0) then
                        
 			-- Création de la nouvelle cellule en debut de liste.
 			Liste := new Cellule'(i, Tableau(i), Liste, Null, Null);
        
        end if;
            
    end loop;
        
end CreationListe;

Procedure TriListe(Liste : in out Pointeur) is
    
--/ Cette procédure accepte une liste triée par ordre décroissant de frequence
--/ et renvoie la liste triée par ordre decroissant de frequence
--/ et de code ASCII.
    
-- Déclaration des pointeurs.  
ListeGauche, ListeDroite, Temporaire : Pointeur := Null;

	procedure TriListeFrequence(Liste : in out Pointeur) is
        
	--/ Cette procédure trie une liste chainée en ordre decroissant
	--/ de fréquence.
      
	-- Déclaration des pointeurs.    
	Grand, Petit, GrandPrecedent, PetitPrecedent, PetitSuivant : Pointeur;
  
	begin
    
		-- Initialisation des pointeurs.
   		Grand := Liste;
   		Petit := Grand.Suivant;
   		GrandPrecedent := Null;
   		PetitPrecedent := Grand;
    
   		while (Grand.Suivant /= Null) loop
        
       		while (Petit /= Null) loop
            
           		PetitSuivant := Petit.Suivant;
            
           		if (Petit.Frequence > Grand.Frequence) then
                
           			if (Grand.Suivant = Petit) then
                    
                   		if (Grand = Liste) then
                        
                       		Petit.Suivant := Grand;
                       		Liste := Petit;
                       		Grand.Suivant := PetitSuivant;
                       		Petit := PetitSuivant;
                       		Grand := Liste;
                       
                   		else
                        
                       		Petit.Suivant := Grand;
                       		GrandPrecedent.Suivant := Petit;
                       		Grand.Suivant := PetitSuivant;
                       		Petit := PetitSuivant;
                       		Grand := GrandPrecedent.Suivant;
                       
                   		end if;
                    
               		else
                        
                   		if (Grand = Liste) then
                        
                       		Petit.Suivant := Grand.Suivant;
                       		Liste := Petit;
                       		Grand.Suivant := PetitSuivant;
                       		PetitPrecedent.Suivant := Grand;
                       		Petit := PetitSuivant;
                       		Grand := Liste;
                       		PetitPrecedent := PetitPrecedent.Suivant;
                        
                   		else
                        
                       		Petit.Suivant := Grand.Suivant;
                       		GrandPrecedent.Suivant := Petit;
                       		Grand.Suivant := PetitSuivant;
                       		PetitPrecedent.Suivant := Grand;
                       		Petit := PetitSuivant;
                       		Grand := GrandPrecedent.Suivant;
                       		PetitPrecedent := PetitPrecedent.Suivant;
                        
                   		end if;
                    
               		end if;
                
           		else
                            
               		Petit := Petit.Suivant;
               		PetitPrecedent := PetitPrecedent.Suivant;
               
   				end if;
    
       		end loop;
        
       		if (GrandPrecedent = Null) then
            
           		GrandPrecedent := Liste;
            
       		else
            
           		GrandPrecedent := GrandPrecedent.Suivant;
            
       		end if;
        
       		Grand := Grand.Suivant;
       		Petit := Grand.Suivant;
       		PetitPrecedent := Grand;
        	
   		end loop;
        
	end TriListeFrequence;

	procedure TriListeCode(Liste : in out Pointeur) is
        
	--/ Cette procédure trie une liste chainée en ordre décroissant
	--/ de code ASCII
      
	-- Déclaration des pointeurs.    
	Grand, Petit, GrandPrecedent, PetitPrecedent, PetitSuivant : Pointeur;
  
	begin
    
		-- Initialisation des pointeurs.
   		Grand := Liste;
   		Petit := Grand.Suivant;
   		GrandPrecedent := Null;
   		PetitPrecedent := Grand;
    
   		while (Grand.Suivant /= Null) loop
        
       		while (Petit /= Null) loop
            
           		PetitSuivant := Petit.Suivant;
            
           		if (Petit.Code > Grand.Code) then
                
           			if (Grand.Suivant = Petit) then
                    
                   		if (Grand = Liste) then
                        
                       		Petit.Suivant := Grand;
                       		Liste := Petit;
                       		Grand.Suivant := PetitSuivant;
                       		Petit := PetitSuivant;
                       		Grand := Liste;
                        
                   		else
                        
                       		Petit.Suivant := Grand;
                       		GrandPrecedent.Suivant := Petit;
                       		Grand.Suivant := PetitSuivant;
                       		Petit := PetitSuivant;
                       		Grand := GrandPrecedent.Suivant;
                        
                   		end if;
                    
               		else
                        
                   		if (Grand = Liste) then
                        
                       		Petit.Suivant := Grand.Suivant;
                       		Liste := Petit;
                       		Grand.Suivant := PetitSuivant;
                       		PetitPrecedent.Suivant := Grand;
                       		Petit := PetitSuivant;
                       		Grand := Liste;
                       		PetitPrecedent := PetitPrecedent.Suivant;
                        
                   		else
                        
                       		Petit.Suivant := Grand.Suivant;
                       		GrandPrecedent.Suivant := Petit;
                       		Grand.Suivant := PetitSuivant;
                       		PetitPrecedent.Suivant := Grand;
                       		Petit := PetitSuivant;
                       		Grand := GrandPrecedent.Suivant;
                       		PetitPrecedent := PetitPrecedent.Suivant;
                        
                   		end if;
                    
               		end if;
                
           		else
                            
               		Petit := Petit.Suivant;
               		PetitPrecedent := PetitPrecedent.Suivant;
               
   				end if;
    
       		end loop;
        
       		if (GrandPrecedent = Null) then
            
           		GrandPrecedent := Liste;
            
       		else
            
           		GrandPrecedent := GrandPrecedent.Suivant;
            
       		end if;
        
       		Grand := Grand.Suivant;
       		Petit := Grand.Suivant;
       		PetitPrecedent := Grand;
        
   		end loop;
    
	end TriListeCode;

	Procedure Scission(Liste : in out Pointeur; Liste1 : out Pointeur; Liste2 : out Pointeur) is
    
	--/ Cette procédure scinde une liste en deux listes:
	--/ Liste1 dont les cellules ont toutes la même fréquence,
	--/ et liste2 le reste de la liste.
    
	Liste2Precedent : Pointeur := Liste;

	begin
    
   		Liste1 := Liste;
   		Liste2 := Liste1.Suivant;
    
   		if (Liste2 /= Null) then
        
       		while (Liste1.Frequence = Liste2.Frequence) loop
            
           		if (Liste2.Suivant /= Null) then
                
               		Liste2 := Liste2.Suivant;
               		Liste2Precedent := Liste2Precedent.Suivant;
                
           		else
                
               		Liste2 := Null;
               		exit;
                
           		end if;
            
       		end loop;
        
       		if (Liste2 /= Null) then
            
           		Liste2Precedent.Suivant := Null;
            
       		end if;
        
   		end if;
    
	end Scission;

	Procedure Concatenation(Liste : in out Pointeur; Liste1 : in Pointeur) is
    
	--/ Cette procédure concaténe deux listes: Liste et Liste1, elle renvoie
	--/ le resultat dans Liste.
 
	Temp : Pointeur := Liste;
    
	begin
        
   		while (Temp.Suivant /= Null) loop
        
       		Temp := Temp.Suivant;
        
   		end loop;
    
   		Temp.Suivant := Liste1;
    
	end Concatenation;
                        
begin
    
    TriListeFrequence(Liste);
    Scission(Liste,ListeGauche,ListeDroite);
    TriListeCode(ListeGauche);
    Liste := ListeGauche;
    Temporaire := ListeDroite;
    
    while (ListeDroite /= Null) loop
        
        Scission(Temporaire, ListeGauche, ListeDroite);
        TriListeCode(ListeGauche);
        Concatenation(Liste,ListeGauche);
        Temporaire := ListeDroite;
        
    end loop;
    
end TriListe;                        

Procedure CreationArbre(Liste : in out pointeur; Arbre : out Pointeur) is
    
--/ Cette procédure crée un arbre à partir d'une liste chainée.
--/ chaque pére pointent sur les deux fils dont la frequence est la moins élevée.
--/ En cas d'égalité on choisit la cellule dont le code ASCII est le plus
--/ petit.
--/ Le pére contient la somme des fréquences des deux fils et le code ASCII le
--/ plus petit.
    
-- Déclaration des pointeurs.
CC, CP, CPP : pointeur;

begin
    
    while (Liste.Suivant /= Null) loop
        
        CC := Liste.Suivant;
        CP := Liste;
        CPP := Null;
        
        while (CC.Suivant /= Null) loop
            
            CC := CC.Suivant;
            CP := CP.Suivant;
            
            if (CPP = Null) then
                
                CPP := Liste;
                
            else
                
                CPP := CPP.Suivant;
                
            end if;
            
        end loop;
        
        if (CC.Code < CP.Code) then
            
            if (CPP /= Null) then
            
        		CPP.Suivant := new cellule'(CC.Code,CC.Frequence + CP.Frequence,Null,CP,CC);
                
            else
                
                Liste := new cellule'(CC.Code,CC.Frequence + CP.Frequence,Null,CP,CC);
                
            end if;
            
        else
                
            if (CPP /= Null) then
                
           		CPP.Suivant := new cellule'(CP.Code,CC.Frequence + CP.Frequence,Null,CP,CC);
                   
            else
                
                Liste := new cellule'(CP.Code,CC.Frequence + CP.Frequence,Null,CP,CC);
                
            end if;
               
        end if;
        
        CP.Suivant := Null;    
        TriListe(Liste);
        
    end loop;
    
    Arbre := Liste;
    
end CreationArbre;

function EstFeuille(Noeud : Pointeur) return boolean is
    
--/ Cette fonction renvoie vrai si le noeud est une feuille
--/ de l'arbre, faux sinon.
    
begin
    
    if (Noeud.FilsGauche = Null and Noeud.FilsDroit = Null) then
        
        return true;
        
    else
        
        return false;
        
    end if;
    
end EstFeuille;

Procedure CreationTable(Arbre : in Pointeur) is
    
--/ Cette procédure crée la table de Huffman en parcourant l'arbre en profondeur.
--/ Par convention on associe la valeur 0 aux arcs de gauche et 1 à ceux
--/ de droite.

            
begin
    
    if (EstFeuille(Arbre)) then                       
        
        for k in 1..(Hauteur - 1) loop
            
        	CaractereCode(Arbre.Code).Binaire(k) := Tampon(k);
            
        end loop;
        
        CaractereCode(Arbre.Code).Longueur := Hauteur - 1;
        
    else
        
       	Tampon(Hauteur) := 0;
        Hauteur := Hauteur + 1;    
        CreationTable(Arbre.FilsGauche);
        Tampon(Hauteur) := 1;
        Hauteur := Hauteur + 1;
        CreationTable(Arbre.FilsDroit);
            
    end if;
    
    Hauteur := Hauteur - 1;
    
end CreationTable;

Procedure Codage is
    
--/ Cette procédure code le fichier source et
--/ écrit le fichier destination (compressé).
    
Caractere : character;    
Freq : String(1..100);
i, m, l : natural;

-- Tampon est vide
k : natural := 1;

	function BinaireDecimale(Octet : Bit) return character is
    
	--/ Cette fonction renvoie le caractère correspondant à
	--/ la séquence binaire de 8 bits contenue dans Octet.
    
	Somme : natural := 0;    
    
	begin 
    
    	for w in 1..8 loop
        
        	Somme := Somme + Octet(w) * (2 ** (8 - w));
        
    	end loop;
    
    	return character'val(Somme);
    
	end BinaireDecimale;
    
begin
    
    Fichier_io.open(Source, Fichier_io.in_file, NomFichierSource(1..LongueurNomSource));
    Fichier_io.create(Destination, Fichier_io.out_file, NomFichierDestination(1..LongueurNomDestination));

    -- Ecriture du repére "Huf".
    Fichier_io.write(Destination,'H');
    Fichier_io.write(Destination,'u');
    Fichier_io.write(Destination,'f');
    NombreCaractereDestination := 3.0;
    
    -- Ecriture du nom du fichier source.
    m := LongueurNomSource; 
    l := 1;
    
    while (((NomFichierSource(m) /= '\') And (NomFichierSource(m) /= '/')) and (m >= 1)) loop
        
        Freq(l) := NomFichierSource(m);
        l := l + 1;
   		m := m - 1;
        
        if (m = 0) then
            
            exit;
            
        end if;
        
    end loop;
    
    for i in reverse 1..(l-1) loop
        	 	    
    	Fichier_io.write(Destination, Freq(i));
        NombreCaractereDestination := NombreCaractereDestination + 1.0;
        
    end loop;
    
    -- Ecriture de la structure du fichier source.
    for i in 0..256 loop
    	
        m := integer'image(Tableau(i))'Length;
        Freq(1..m) := integer'image(Tableau(i));
        
        for w in 1..m loop
        
        	Fichier_io.write(Destination, Freq(w));
            NombreCaractereDestination := NombreCaractereDestination + 1.0;
            
        end loop;    
        
    end loop;    
    
	while not Fichier_io.end_of_file(Source) loop
        
        Fichier_io.read(Source, Caractere); 
        i := 1;
        
        while (i <= CaractereCode(character'pos(Caractere)).Longueur) loop
            
            if (k > 8) then
                
            	Fichier_io.write(Destination, BinaireDecimale(Tampon));
                NombreCaractereDestination := NombreCaractereDestination + 1.0;
                k := 1;
                
            end if;
            
            Tampon(k) := CaractereCode(character'pos(Caractere)).Binaire(i);
            k := k + 1;
            i := i + 1;
            
        end loop;
        
    end loop;
    
    i := 1;
    
    while (i <= CaractereCode(256).Longueur) loop
        
        if (k > 8) then
            
        	Fichier_io.write(Destination, BinaireDecimale(Tampon));
            NombreCaractereDestination := NombreCaractereDestination + 1.0;
            k := 1;
            
        end if;
        
        Tampon(k) := CaractereCode(256).Binaire(i);
        k := k + 1;
        i := i + 1;
        
    end loop;
    
    if (k <= 8) then
        
        for j in k..8 loop
            
            Tampon(j) := 0;
            
        end loop;
                
    end if;
    
    Fichier_io.write(Destination, BinaireDecimale(Tampon));
    NombreCaractereDestination := NombreCaractereDestination + 1.0;
    
    Fichier_io.close(Source);
    Fichier_io.close(Destination);
    
end Codage;

Procedure FormatFichier is
    
--/ Cette procédure vérifie si le fichier à décompresser
--/ est au format projet.
    
Caractere : character;    
    
begin
    
	Fichier_io.open(Source, Fichier_io.in_file, NomFichierSource(1..LongueurNomSource));
    Fichier_io.read(Source, Caractere);
    
    -- Vérification du format de fichier.
    if (Caractere /= 'H') then
        
        Fichier_io.close(Source);
        raise MAUVAIS_FORMAT;
        
    end if;
    
    Fichier_io.read(Source, Caractere);
    
    if (Caractere /= 'u') then
        
        Fichier_io.close(Source);
        raise MAUVAIS_FORMAT;
        
    end if;
    
    Fichier_io.read(Source, Caractere);
    
    if (Caractere /= 'f') then
        
        Fichier_io.close(Source);
        raise MAUVAIS_FORMAT;
        
    end if;
    
end FormatFichier;    

Procedure NomOriginal is

--/ Cette procédure récupére le nom du fichier original

Caractere : character;        

begin
    
	Fichier_io.read(Source, Caractere);
    LongueurNomDestination := LongueurNomDestination + 1;
    
    while (Caractere /= ' ') loop
        
        NomFichierDestination(LongueurNomDestination) := Caractere;
        Fichier_io.read(Source, Caractere);
        LongueurNomDestination := LongueurNomDestination + 1;
        
    end loop;
    
    LongueurNomDestination := LongueurNomDestination - 1;
    
end NomOriginal;        

Procedure ChargementStructure(Tableau : out Vecteur) is
    
--/ Cette procédure charge dans Tableau la structure 
--/ du fichier original.

Caractere : character;    
Temp : string(1..10);
j : natural := 0;
    
begin
    
	For i in 0..256 loop
        
    	Fichier_io.read(Source, Caractere);
        j := 1;
        
        while (Caractere /= ' ') loop
            
            if (i = 256) then
              
            	Temp(j) := Caractere;
                j := j + 1;
                exit;
                
            end if;	
                      
        	Temp(j) := Caractere;
            Fichier_io.read(Source, Caractere);
            j := j + 1;
            
        end loop;
        
        j := j - 1;
        Tableau(i) := integer'value(Temp(1..j));	
        
    end loop;	                
        
end ChargementStructure;

Procedure Decodage(Arbre : in Pointeur) is
    
--/ Cette procédure décode le fichier compressé
--/ et ecrit le resultat dans le fichier original.
    
Fin : Boolean := false;
Noeud : Pointeur := Arbre;
j : natural := 1;
Caractere : character;

	Procedure DecimaleBinaire(Caractere : in character; Tampon : out Bit) is
    
	--/ Cette procédure convertit un caractère en sequence binaire de 8 bits.

	Code : natural;        
	i : natural := 9;
    
	begin
    
		Code := character'pos(Caractere);	            
    
    	while (Code /= 0) loop
        
        	i := i - 1;
        	Tampon(i) := Code mod 2;
        	Code := Code / 2;
        
    	end loop;
    
    	if (i > 1) then
        
        	for w in 1..(i-1) loop
            
            	Tampon(w) := 0;
            
        	end loop;
        
    	end if;
    
	end DecimaleBinaire;

begin
    
    Fichier_io.create(Destination, Fichier_io.out_file, NomFichierDestination(1..LongueurNomDestination));
    
    Fichier_io.read(Source, Caractere);
    DecimaleBinaire(Caractere, Tampon);
        
    While not(Fin) loop

    	While not(EstFeuille(Noeud)) loop 
    	
        	if (Tampon(j) = 0) then
                
                Noeud := Noeud.FilsGauche;
                
            else
                
                Noeud := Noeud.FilsDroit;
                
            end if;
            
            j := j + 1;
            
            if (j = 9) then
                
                if (not(Fichier_io.End_Of_File(Source))) then
                    
                	Fichier_io.read(Source, Caractere);
                    DecimaleBinaire(Caractere, Tampon);
                    j := 1;
                    
                end if;
                
            end if;
            
        end loop;
        
        if (Noeud.Code = 256) then
            
            Fin := true;
            
        else
            
            Fichier_io.write(Destination, character'val(Noeud.Code));
            Noeud := Arbre;
            
    	end if;        
    
    end loop;
    
    Fichier_io.close(Source);
    Fichier_io.close(Destination);
    
end Decodage;

--/--------------------/
--/ Corps du programme./
--/--------------------/                                                                
begin
    
    if (Argument_Count < 2 or Argument_Count > 3) then
        
        raise CONSTRAINT_ERROR;
        
    end if;
    
    -- Compression.
    if (Argument(1) = "c") then
        
    	LongueurNomSource := Argument(2)'length;
    	NomFichierSource(1..LongueurNomSource) := Argument(2);
    
    	LongueurNomDestination := Argument(3)'length;
    	NomFichierDestination(1..LongueurNomDestination) := argument(3);
    
        new_line;
        put_line("Compression du fichier " & NomFichierSource(1..LongueurNomSource) & " en cours, veuillez patienter...");
        
        TimeDebut := Clock;
        
    	ChargementCaractere(Tableau);
    	CreationListe(Liste,Tableau);
    	TriListe(Liste);
    	CreationArbre(Liste,Arbre);
    	CreationTable(Arbre);
    	Codage;
        
        TimeFin := Clock;
        
        new_line;
        put_line("L'opération s'est terminée avec succés.");
        put("Le taux de compression du fichier " & NomFichierDestination(1..LongueurNomDestination) & " est: ");
        put_line(integer'image(integer(abs(100.0 - ((NombreCaractereDestination/(NombreCaractereSource + 1.0)) * 100.0)))) & "%");
        put_line("L'opération a duré" & integer'image(integer(TimeFin - TimeDebut)) & " secondes.");
        
    else 
    
        -- Décompression.
    	if (Argument(1) = "d") then
            
    		LongueurNomSource := Argument(2)'length;
    		NomFichierSource(1..LongueurNomSource) := Argument(2);
            
            if (argument_count = 3) then
                
                LongueurNomDestination := Argument(3)'length;
	    		NomFichierDestination(1..LongueurNomDestination) := argument(3);
                
            else
                
                LongueurNomDestination := 0;
                
            end if;
            
            FormatFichier;
            NomOriginal;
            
            new_line;
        	put_line("Décompression du fichier " & NomFichierSource(1..LongueurNomSource) & " en cours, veuillez patienter...");

            TimeDebut := Clock;
            
            ChargementStructure(Tableau);
            CreationListe(Liste,Tableau);
    		TriListe(Liste);
    		CreationArbre(Liste,Arbre);
            Decodage(Arbre);
            
            TimeFin := Clock;
            
            new_line;
	        put_line("L'opération s'est terminée avec succés.");
            put_line("L'opération a duré" & integer'image(integer(TimeFin - TimeDebut)) & " secondes.");

        else
            
            AfficheAide;
            
        end if;
        
    end if;    
        
    exception
        WHEN NAME_ERROR => put_line("Chemin invalide."); 
        WHEN DEVICE_ERROR => put_line("Chemin invalide.");
        WHEN CONSTRAINT_ERROR => AfficheAide;
        WHEN USE_ERROR => put_line("Chemin introuvable.");
        WHEN MAUVAIS_FORMAT => put_line("Le fichier " & NomFichierSource(1..LongueurNomSource) & " n'est pas un fichier compressé.");
                
end Projet;

With types;

package code is

   
    procedure Tableau_code (arbre : in T_arbre; tab_code : out T_tab_code) with
            Pre => arbre'first = 1 and arbre'last >= 1 ;
    
    
    procedure Traduire (fichier : in string; code : out T_code) with
            Pre => fichier /= Null,
            Post => code'first = 1 and code'last >= 1;
                       
    
end code;
                    

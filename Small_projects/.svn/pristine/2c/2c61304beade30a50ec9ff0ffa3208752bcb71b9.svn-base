with types;	use types;

package decode is

	Procedure Lire (fichier : in string; code : out T_code; tab_code : out T_tab_code; lng_decode : out integer) with
	Pre => fichier /= Null,
	Post => code'first = 1 and code'last <= 1 and lng_decode > 0 and tab_code'first = 1 and tab_code'last = 256;


	Procedure Decoder (code : in T_code; tab_code : in T_tab_code; lng_decode : in integer; decode : out T_decode) with
	Pre => code'first = 1 and code'last <= 1 and lng_decode < 0 and tab_code'first = 1 and tab_code'last = 256,
	Post => (decode'first - decode'last) = lng_decode;


	Procedure Ecrire (decode : in T_decode) with
	Pre => decode /= Null;

end decode;

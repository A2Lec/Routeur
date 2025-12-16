
package body IP is




procedure Masquer (premier_str) is

    premier_modif : String;
begin

	puissance := 0;
	premier := Integer'Value(premier_str);

	while premier mod 2 != 1 loop

		puissance := puissance + 1;
		premier := premier / 2;

	end loop;

	premier_modif := Integer’Image(Integer'Value(premier_str) - 2 ** puissance);

end Masquer;


procedure Masquer_bit (Adresse : in T_IP; Adresse_m : out T_IP)

    procedure Valeur_A_Masquer (Adresse : in T_IP; Indice : out Integer; Valeur : out Integer) is
        Taille : Integer;
        Adresse_Str : String;
        Premier_Str : String;
        Chaine_Entier : String;
    begin
        Adresse_Str := To_String (Adresse);
        Taille := Length(Adresse);
        Indice := 1;
        Premier_Str := “”;
        Chaine_Entier := “”;

        for i in 1..Taille loop

            if Adresse_Str(i) /= “.” then
                Chaine_Entier := Chaine_Entier & Adresse(i);

            elsif Chaine_Entier /= “0” then
                Indice := i - Chaine_Entier’Length;
                Premier_Str := Chaine_Entier;
                Chaine_Entier := “”;

            else
                Chaine_Entier := “”;

            end if;
        end loop;

        Valeur := Integer'Value(Premier_Str);

    end Valeur_A_Masquer;

    procedure Modifie_Entier () is
        Puissance : Integer;

    begin


    end Modifie_Entier;

begin


end Masquer_bit;



end IP;

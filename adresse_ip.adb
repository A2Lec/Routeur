
package body IP is



    procedure Masquer_bit (Adresse : in out T_IP) is

        procedure Valeur_A_Masquer (Adresse_Str : in String; Indice : out Integer; Valeur : out Integer) is
            Taille : Integer;
            Premier_Str : String;
            Chaine_Entier : String;
        begin
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

        function Modifie_Entier (Premier : Integer) return Integer is

            Puissance : Integer;
            Valeur_Mod : Integer;
        begin
            Puissance := 0;
            Valeur_Mod := Premier;
            while Valeur_Mod mod 2 != 1 loop

                Puissance := Puissance + 1;
                Valeur_Mod := Valeur_Mod / 2;

            end loop;
            return Premier - 2 ** puissance;

        end Modifie_Entier;

        Indice : Integer;
        Adresse_Str : String;
        Valeur : Integer;
        Valeur_Str : String;

    begin
        Adresse_Str := To_String (Adresse);
        Valeur_A_Masquer (Adresse_Str, Indice, Valeur);
        Valeur := Modifie_Entier (Valeur);
        Valeur_Str = Integer'Image(Valeur);
        Adresse_Str := Adresse_Str(1 .. indice - 1) & Valeur_Str & Adresse_Str (Indice + Valeur_Str’Length .. Adresse_Str‘Last);
        Adresse := To_Unbounded_String (Adresse_Str);

    end Masquer_bit;


    procedure Masquer_adresse (Adresse : in out T_IP; Masque : in T_Masque)

    begin
        -- difficulté : trouver combien de fois il faut l'appliquer
        -- modifier la procédure précédente ?


    end Masquer_adresse;


end IP;



Proposition Alexis :

function Longueur_masque(Masque : in Unbounded_String) is
    compteur : Integer := 0;
    bit_test : T_Adresse_IP := 2 ** 31;
begin
    for i in 1..32 loop
        if (Masque and bit_test) /= 0 then
            compteur := compteur + 1;
        end if;
        bit_test := bit_test /2;
    end loop;
    return compteur;

end;

function Trouver_route_optimale(Table : in T_Table; IP_donnee : in T_Adresse_IP) return T_Adresse_IP is
    IP_opti : T_Adresse_IP := 0;

    Max_Longueur : Integer := -1;

    IP_Calculee  : T_Adresse_IP;
    Len_Actuelle : Integer;

    Masque_str : Unbounded_String;
    IP_masque_str : Unbounded_String;

    Masque_Val : T_Adresse_IP;
    IP_masque_Val : T_Adresse_IP;

begin

    for ligne in 1..Taille(Table) loop

        Masque := recuperer_masque(Table, ligne);
        -- IP_masque := recuperer_IP(Table, ligne);

        IP_Calculee := IP_donnee and Masque;

        if IP_Calculee = IP_masque then

            Len_Actuelle := Longueur_masque(Masque_Val);

            if Len_Actuelle > Max_Longueur then
                Max_Longueur := Len_Actuelle;
                IP_opti      := IP_Calculee;

            end if;

        end if;

    end loop;

    return IP_opti;
end Trouver_route_optimale;

function Text_to_IP(Text : in Unbounded_String) return T_Adresse_IP is
    Resultat : T_Adresse_IP := 0;
    Partie   : Unbounded_String;
    Pos      : Integer := 1;
    Octet_Val : T_Octet;
begin
    for i in 0..3 loop
        Partie := "";
        while Pos <= Length(Text) and then Text(Pos) /= '.' loop
            Partie := Partie & Text(Pos);
            Pos := Pos + 1;
        end loop;

        Octet_Val := T_Octet(Integer'Value(To_String(Trim(Partie))));
        Resultat := Resultat * 256 + T_Adresse_IP(Octet_Val);

        Pos := Pos + 1;
    end loop;
    return Resultat;
end Text_to_IP;

function recuperer_masque(Table : in T_Table; ligne : in Integer) return T_Adresse_IP is
    Masque_str : Unbounded_String;
begin
    Masque_str := Get_Champ(Table, ligne, 2);
    return Text_to_IP(Masque_str);
end recuperer_masque;

function recuperer_IP(Table : in T_Table; ligne : in Integer) return T_Adresse_IP is
    IP_str : Unbounded_String;
begin
    IP_str := Get_Champ(Table, ligne, 1);
    return Text_to_IP(IP_str);
end recuperer_IP;

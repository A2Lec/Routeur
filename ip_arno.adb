with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;

package body IP is



    function Modifie_Entier (Entier : Integer) return Integer is

        Puissance : Integer;
        Valeur_Mod : Integer;

    begin
        Puissance := 0;
        Valeur_Mod := Entier;
        while Valeur_Mod mod 2 /= 1 loop

            Puissance := Puissance + 1;
            Valeur_Mod := Valeur_Mod / 2;

        end loop;
        return Entier - 2 ** puissance;

    end Modifie_Entier;

    function Ub_To_Ip (Chaine : Unbounded_String) return T_IP is
    begin
	return T_IP(Chaine);
    end Ub_To_Ip;

    function Ip_To_Ub (Adresse : T_IP) return Unbounded_String is
    begin 
	return Unbounded_String(Adresse);
    end Ip_To_Ub;

   
    procedure Kieme_Adresse (K : in Integer; Adresse : in T_IP; Indice : out Integer; Valeur : out Integer) is
        Copie_Adresse : String := To_String (Adresse);
        Copie_Indice : Integer;
        Taille : Integer;
        Valeur_Str : Unbounded_String;
    begin

        Taille := Copie_Adresse'Length;
        Indice := 1;

        for i in 2..K loop
            while Copie_Adresse(Indice) /= '.' loop
                Indice := Indice + 1;
            end loop;
	    Indice := Indice + 1;
        end loop;

        Copie_Indice := Indice;
        Valeur_Str := To_Unbounded_String("");
        while Copie_Indice <= Taille and then Copie_Adresse(Copie_Indice) /= '.' loop
            Valeur_Str := To_Unbounded_String(To_String(Valeur_Str) & Copie_Adresse(Copie_Indice));
            Copie_Indice := Copie_Indice + 1;
        end loop;

        Valeur := Integer'Value(To_String(Valeur_Str));

    end Kieme_Adresse;

    procedure Adresse_Non_Nul (Adresse : in T_IP; Indice : out Integer; Valeur : out Integer) is

        Compteur : Integer;
    begin
        Compteur := 4;

        loop

	    

            Kieme_Adresse (Compteur, Adresse, Indice, Valeur);
            Compteur := Compteur - 1;
            exit when Valeur /= 0;
        end loop;
    end Adresse_Non_Nul;



    procedure Masquer_bit (Adresse : in out T_IP) is

        Indice : Integer;
        Adresse_Str : String := To_String (Adresse);
        Valeur : Integer;
        Valeur_Str : Unbounded_String;

    begin
        Adresse_Non_Nul (Adresse, Indice, Valeur);
        Valeur := Modifie_Entier (Valeur);
        Valeur_Str := To_Unbounded_String(Integer'Image(Valeur));
        Adresse_Str := Adresse_Str(1 .. indice - 1) & To_String(Valeur_Str) & Adresse_Str (Indice + Length(Valeur_Str) .. Adresse_Str'Last);
        Adresse := To_Unbounded_String (Adresse_Str);

    end Masquer_bit;


    function Zero_Entier (Valeur : Integer) return Integer is
        Nb : Integer;
        Copie : Integer;
    begin
        Nb := 0;
        Copie := Valeur;
        while Copie mod 2 ** (Nb + 1) = 0 loop

	    Put(Integer'Image(Nb));

            Nb := Nb + 1;
        end loop;

        return Nb;
    end Zero_Entier;

 
    function Adresse_Zero_Bit (Adresse : T_IP) return Integer is

        Nb : Integer;
        Indice : Integer;
        Valeur :Integer;

    begin
        Adresse_Non_Nul (Adresse, Indice, Valeur);
        Nb := Zero_Entier (Valeur);
        Nb := Nb + (Length(Ip_To_Ub(Adresse)) - (Indice + Integer'Image (Valeur)'Length - 1)) * 8;
	
	Put("Adresse");
	Put(To_String(Adresse));
	Put ("longueur adresse");
	Put (Length(Ip_To_Ub(Adresse)));
	New_Line;
	Put("Indice");
	Put(Integer'Image(Indice));
	New_Line;
	Put("Valeur");
	Put(Integer'Image(Integer'Image (Valeur)'Length));
	New_Line;

        return Nb;
    end Adresse_Zero_Bit;


    procedure Masquer_Adresse (Adresse : in out T_IP; Masque : in T_IP) is
        Nb_Masque : Integer;
        Nb_Adresse : Integer;
        Iterations : Integer;
    begin
        Nb_Masque := Adresse_Zero_Bit (Masque);

	Put(To_String(Masque));
	New_Line;
	Put(Integer'Image(Nb_Masque));
	New_Line;

        Nb_Adresse := Adresse_Zero_Bit (Adresse);

	Put(To_String(Adresse));
	New_Line;
	Put(Integer'Image(Nb_Adresse));
	New_Line;

        Iterations := Nb_Masque - Nb_Adresse;
	if Iterations > 0 then
		for i in 1..Iterations loop
			 Masquer_bit (Adresse);
		end loop;
	else 
		Null;
	end if;

    end Masquer_Adresse;


end IP;
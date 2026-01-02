with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings;               use Ada.Strings;

package body Adresse_IP is

    function Modifie_Entier (Entier : Integer) return Integer is

        Puissance : Integer;

    begin
        Puissance := Zero_Entier(Entier);
        return Entier - 2 ** Puissance;

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
        Copie_Adresse : constant String := To_String (Adresse);
        Indice_Fin : Integer;
        Taille : Integer;

    begin
        -- Put_Line("Kieme_Adresse K=" & Integer'Image(K) & " Adr=" & Copie_Adresse);
        Taille := Copie_Adresse'Length;
        Indice := 1;

        for i in 2..K loop
            while Indice <= Taille and then Copie_Adresse(Indice) /= '.' loop
                Indice := Indice + 1;
            end loop;
	    Indice := Indice + 1;
        end loop;

        Indice_Fin := Indice;
        while Indice_Fin <= Taille and then Copie_Adresse(Indice_Fin) /= '.' loop
            Indice_Fin := Indice_Fin + 1;
        end loop;

	Indice_Fin := Indice_Fin - 1;

        Valeur := Integer'Value (Copie_Adresse (Indice .. Indice_Fin));

    end Kieme_Adresse;

    procedure Adresse_Non_Nul (Adresse : in T_IP; Indice : out Integer; Valeur : out Integer) is

        Compteur : Integer;
    begin

        Compteur := 4;

        loop
            -- Put_Line("Adresse_Non_Nul Compteur=" & Integer'Image(Compteur));
            Kieme_Adresse (Compteur, Adresse, Indice, Valeur);
	   
            Compteur := Compteur - 1;
            exit when Valeur /= 0 or Compteur < 0;
        end loop;
    end Adresse_Non_Nul;



    procedure Masquer_bit (Adresse : in out T_IP) is

        Indice : Integer;
        Adresse_Str : constant String := To_String (Adresse);
        Valeur : Integer;
	Valeur_Str_Init : Unbounded_String;
        Valeur_Str : Unbounded_String;
	Debut : Unbounded_String;
	Fin : Unbounded_String;

    begin

        Adresse_Non_Nul (Adresse, Indice, Valeur);

	Valeur_Str_Init := To_Unbounded_String(Trim (Integer'Image (Valeur), Left));
        Valeur := Modifie_Entier (Valeur);
        Valeur_Str := To_Unbounded_String (Trim (Integer'Image (Valeur), Left));
	
	if Adresse_Str'First <= Indice - 1 then 
		Debut := To_Unbounded_String(Adresse_Str(Adresse_Str'First .. Indice - 1));
	else
		Debut := To_Unbounded_String("");
	end if;

	if Indice + Length(Valeur_Str_Init) <= Adresse_Str'Last then
		Fin := To_Unbounded_String(Adresse_Str (Indice + Length(Valeur_Str_Init) .. Adresse_Str'Last));
	else
		Fin := To_Unbounded_String("");
	end if;

        Adresse := To_Unbounded_String (To_String(Debut) & To_String(Valeur_Str) & To_String(Fin));

    end Masquer_bit;


    function Zero_Entier (Valeur : Integer) return Integer is
        Nb : Integer;
        Copie : Integer;
    begin
        Nb := 0;
        Copie := Valeur;
        while Copie mod 2 ** (Nb + 1) = 0 loop
            Nb := Nb + 1;
        end loop;

        return Nb;
    end Zero_Entier;


	 
    function Adresse_Zero_Bit (Adresse : T_IP) return Integer is

        Nb : Integer;
        Indice : Integer;
        Valeur : Integer;
	Valeur_Str : Unbounded_String;

    begin
        Adresse_Non_Nul (Adresse, Indice, Valeur);
        if Valeur = 0 then
            return 32;
        end if;
	Valeur_Str := To_Unbounded_String(Trim (Integer'Image (Valeur), Left));
        Nb := Zero_Entier (Valeur);
        Nb := Nb + 4 * (Length(Ip_To_Ub(Adresse)) - (Indice + Length(Valeur_Str) - 1)) ;
        return Nb;
    end Adresse_Zero_Bit;


    procedure Masquer_Adresse (Adresse : in out T_IP; Masque : in T_IP) is
        Val_A, Val_M, Res : Integer;
        Indice_A, Indice_M : Integer;
        Nouvelle_Adresse : Unbounded_String := To_Unbounded_String("");
        type Byte is mod 256;
    begin
        for K in 1..4 loop
            Kieme_Adresse(K, Adresse, Indice_A, Val_A);
            Kieme_Adresse(K, Masque, Indice_M, Val_M);
            
            Res := Integer(Byte(Val_A) and Byte(Val_M));

            if K > 1 then
                Nouvelle_Adresse := Nouvelle_Adresse & ".";
            end if;
            Nouvelle_Adresse := Nouvelle_Adresse & Trim(Integer'Image(Res), Left);
        end loop;
        
        Adresse := Ub_To_Ip(Nouvelle_Adresse);
    end Masquer_Adresse;

end Adresse_IP;

with Ada.Strings;               use Ada.Strings;	-- pour Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with Ada.Strings.Fixed;


package body Routeur is

    Table : Unbounded_String;
    Paquets : Unbounded_String;
    Resultats : Unbounded_String;
    Table_Routage : T_LCA;


    function Get_Champ (Fichier : File_Type) return Unbounded_String is
        Champ : Unbounded_String;
        C : Character;
    begin

        Champ := To_Unbounded_String ("");

        loop

            exit when End_Of_File(Fichier);
            Get(Fichier, C);

            if C = ' ' or C = ASCII.HT or C = ASCII.LF then
                exit when Length (Champ) > 0;
            else
                Champ := Champ & C;
            end if;

        end loop;

        return To_Unbounded_String(Ada.Strings.Fixed.Trim(To_String(Champ), Both));

   end Get_Champ;

    procedure Recuperer_Arguments (Table : out Unbounded_String; Paquets : out Unbounded_String; Resultats : out Unbounded_String) is

    begin

        Table := To_Unbounded_String("table.txt");
        Paquets :=  To_Unbounded_String("paquets.txt");
        Resultats := To_Unbounded_String ("resultats.txt");

        for indice in 1 .. Argument_Count loop

            if Argument (indice) = "-t" then
                Table :=  To_Unbounded_String(Argument (indice + 1));

            elsif Argument (indice) = "-q" then
                Paquets :=  To_Unbounded_String(Argument (indice + 1));

            elsif Argument (indice) = "-r" then
                Paquets :=  To_Unbounded_String(Argument (indice + 1));

            else
                Null;

            end if;
        end loop;

    end Recuperer_Arguments;


    procedure Construire_Table (Table_Routage : out T_LCA; Table : in Unbounded_String) is

        Entree_Table : File_Type;
        Destination : T_IP;
        Masque : T_IP;
        Inter : Unbounded_String;

    begin

        Open (Entree_table, In_File, To_String (Table));
        Initialiser (Table_Routage);

        loop

            Destination := Ub_To_Ip(Get_Champ (Entree_Table));
	    Masque := Ub_To_Ip(Get_Champ (Entree_Table));
	    Inter := Get_Champ (Entree_Table);
	    
	    Masquer_Adresse (Destination, Masque);
            Enregistrer (Table_Routage, Destination, Inter);
            exit when End_Of_File (Entree_Table);

        end loop;

        Close (Entree_Table);

    end Construire_Table;


	function Interface_Sortie (Adresse : T_IP; Table_Routage : T_LCA) return Unbounded_String is
		Copie_Adresse : T_IP;
	begin
		Copie_Adresse := Adresse;
        	while not Cle_Presente (Table_Routage, Copie_Adresse) loop

            		 Masquer_Bit (Copie_Adresse);

                end loop;

		return La_Valeur (Table_Routage, Copie_Adresse);

	end Interface_Sortie;

    procedure Afficher_Ip (Adresse : in T_IP) is
    begin

	Put(To_String(Ip_To_Ub(Adresse)));

    end Afficher_Ip;

    procedure Afficher_Ub (Inter : in Unbounded_String) is
    begin

	Put(To_String(Inter));

    end Afficher_Ub;

    procedure Afficher is new Afficher_Diagnostic (Afficher_Cle => Afficher_Ip, Afficher_Donnee => Afficher_Ub);

    procedure Traiter_Ligne_Paquets (Ligne_p : in Unbounded_String; Num_Ligne : in Integer; Sortie_Resultats : in out File_Type; Continuer : in out Boolean; Table_Routage : in T_LCA) is

    Inter : Unbounded_String;

    begin

        if To_String(Ligne_p) = "table" then
            Put (Sortie_resultats, "table (ligne " & Integer'Image (Num_Ligne) & ")");
            New_Line (Sortie_Resultats);
            Afficher (Table_Routage);
            New_Line (Sortie_resultats);

        elsif To_String(Ligne_p) = "fin" then
            Continuer := false;

        else
            Inter := Interface_Sortie (Ub_To_Ip(Ligne_p), Table_Routage);
            Put (Sortie_resultats, Ligne_p & Inter);
            New_Line (Sortie_resultats);

        end if;

    end Traiter_Ligne_Paquets;



    procedure Traiter_Fichiers_Paquets (Paquets : in Unbounded_String; Resultats : in Unbounded_String; Table_Routage : in T_LCA) is

    Entree_Paquets : File_Type;
    Sortie_Resultats : File_Type;
    Continuer : Boolean;
    Num_Ligne : Integer;
    Ligne_p : Unbounded_String;

    begin

        Open (Entree_Paquets, In_File, To_String (Paquets));
        Create (Sortie_Resultats, Out_File, To_String (Resultats));
        Continuer := true;

        while not (End_Of_File (Entree_Paquets)) and Continuer loop

            Num_Ligne :=  Integer (Line (Entree_paquets));
            Ligne_p := Get_Line (Entree_paquets);
            Traiter_Ligne_Paquets (Ligne_p, Num_Ligne, Sortie_Resultats, Continuer, Table_Routage);

        end loop;


        Close (Entree_paquets);
        Close (Sortie_resultats);

    end Traiter_Fichiers_Paquets;

end Routeur;

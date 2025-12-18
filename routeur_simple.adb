with Ada.Strings;               use Ada.Strings;	-- pour Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with LCA;

procedure Routeur_Simple is

    package Tab_Routage is new LCA (T_Cle => Unbounded_String, T_Valeur => Unbounded_String);
    use Tab_Routage;

    Table : Unbounded_String;
    Paquets : Unbounded_String;
    Resultats : Unbounded_String;
    Table_Routage : T_LCA;

begin

    procedure Recup_Arguments (Table : in out Unbounded_String; Paquets in out Unbounded_String; Resultats : in out Unbounded_String) is

    begin

        Table := To_Unbounded_String(“table.txt”);
        Paquets :=  To_Unbounded_String(“paquets.txt”);
        Resultats := To_Unbounded_String (“resultats.txt”);

        for indice in 1 .. Argument_Count loop

            if Argument (indice) = “-t” then
                Table :=  To_Unbounded_String(Argument (indice + 1));

            elsif Argument (indice) = “-q” then
                Paquets :=  To_Unbounded_String(Argument (indice + 1));

            elsif Argument (indice) = “-r” then
                Paquets :=  To_Unbounded_String(Argument (indice + 1));

            else
                Null;

            end if;
        end loop;
    end Recup_Arguments;


    procedure Construit_Table (Table_Routage : out T_LCA; Table : in Unbounded_String) is

        Entree_Table : File_Type;
        Destination : Unbounded_String;
        Masque : Unbounded_String;
        Interface : Unbounded_String;

    begin

        Open (Entree_table, In_File, To_String (Table));
        Initialiser (Table_Routage);

        loop

            Get (Entree_Table, Destination);
            Get (Entree_Table, Masque);
            Get (Entree_Table, Interface);
            Enregistrer (Table_Routage, Destination_Masquee, Interface);
            exit when End_Of_File (Entree_Table);

        end loop;

        Close (Entree_Table);

    end Construit_Table;







    procedure Traiter_Ligne_Paquets (Ligne_p : in Unbounded_String; Num_Ligne : in Integer; Sortie_Resultats : in out File_Type; Continuer : in out Boolean; Table_Routage : in T_LCA) is

    Interface : Unbounded_String;

    begin

        Case To_String(Ligne_p) is

        when “table” =>
            Put (Sortie_resultats, “table (ligne ” & Integer'Image (Num_Ligne) & “)”);
            New_Line (Sortie_Resultats);
            Afficher (Table_Routage);
            New_Line (Sortie_resultats);

        when “fin” =>
            Continuer := false;

        when others =>
            Interface := Interface_Sortie ();
            Put (Sortie_resultats, Ligne_p & Interface);
            New_Line (Sortie_resultats);

        end Case;

    end Traiter_Ligne_Paquets;





    procedure Construit_Resultat (Paquets : in Unbounded_String; Resultats : in Unbounded_String; Table_Routage : in T_LCA) is

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
            Get (Entree_paquets, Ligne_p);
            Traiter_Ligne_Paquets (Ligne_p, Num_Ligne, Sortie_Resultats, Continuer, Table_Routage);

        end loop;


        Close (Entree_paquets);
        Close (Sortie_resultats);

    end Construit_Resultat;

    Recup_Arguments (Table, Paquets, Resultats);
    Construit_Table (Table_Routage, Table);
    Construit_Resultat (Paquets, Resultats, Table_Routage);

end Routeur_Simple;



procedure Interface_Sortie (Ligne_p; Table) is
begin

        ip_masquee := To_String (ligne_p);
        while not Cle_Presente (table_routage, To_String(ip_masquee)) loop
            taille := ip_masquee’Length;
            Trouver l’indice du premier entier non nul de ip_masquee et cet entier

            Masquer le premier bit à “1” de cet entier;

            ip_masquee := ip_masquee(1 .. indice - 1) & premier_modif & ip_masquee (indice + premier_str’Length .. ip_masquee‘Last);

                end loop;

	interface := La_Valeur (table_routage, To_Unbounded_String(ip_masquee));

end Interface_Sortie;




procedure Valeur_A_Masquer (Ligne_p; Table) is
begin

    indice := 1;
	premier_str := “”;
	chaine_entier := “”

	Pour i allant de 1 à taille loop

		if ip_masquee (i) != “.” Alors
			chaine_entier := chaine_entier & ip_masquee (i);

		elsif chaine_entier != “0” Alors
			indice := i - chaine_entier’Length;
			premier_str := chaine_entier;
			chaine_entier := “”;

		else
			chaine_entier := “”;
		end if;
	end loop;

end Valeur_A_Masquer;


procedure Masquer (premier_str) is
    puissance : Integer;
    premier : Integer;
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

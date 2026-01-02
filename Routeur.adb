with Ada.Strings;               use Ada.Strings;	-- pour Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

package body Routeur is

    Table : Unbounded_String;
    Paquets : Unbounded_String;
    Resultats : Unbounded_String;
    Table_Routage : T_LCA;

    procedure Afficher_Une_Route (Destination : in T_IP; Route : in T_Route) is
    begin
        Put(To_String(Ip_To_Ub(Destination)));
        Put(" ");
        Put(To_String(Ip_To_Ub(Route.Masque)));
        Put(" ");
        Put(To_String(Route.Inter));
        New_Line;
    end Afficher_Une_Route;

    procedure Afficher_Table is new Tab_Routage.Faire_Pour_Chaque (Traiter => Afficher_Une_Route);


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
                Resultats :=  To_Unbounded_String(Argument (indice + 1));

            else
                Null;

            end if;
        end loop;

    end Recuperer_Arguments;



    procedure Construire_Table (Table_Routage : out T_LCA; Table : in Unbounded_String) is

        Entree_Table : File_Type;
        Destination  : Unbounded_String;
        Masque       : Unbounded_String;
        Interface_S  : Unbounded_String;
        Line_Content : Unbounded_String;
        Pos1, Pos2   : Integer;
        Destination_Masquee : T_IP;

    begin

        Open(Entree_Table, In_File, To_String(Table));

        Tab_Routage.Initialiser(Table_Routage);

        while not End_Of_File(Entree_Table) loop
            begin
                Line_Content := To_Unbounded_String(Get_Line(Entree_Table));
                Line_Content := To_Unbounded_String(Trim(To_String(Line_Content), Both));

                if Length(Line_Content) > 0 then

                    -- Format attendu : "destination masque interface" 
                    -- On va découper la ligne en fonction des espaces.*
                    Pos1 := 1;
                    Pos2 := Index(To_String(Line_Content), " ");
                    if Pos2 > 0 then

                        -- On récupère la destination (avant le premier espace)
                        Destination := Unbounded_Slice(Line_Content, Pos1, Pos2 - 1);
                        Pos1 := Pos2 + 1;

			while To_String(Line_Content) (Pos1) = ' ' loop
			    Pos1 := Pos1 + 1;
			end loop;

                        Pos2 := Index(To_String(Line_Content), " ", Pos1);
                        if Pos2 > 0 then

                            -- On récupère le masque (entre premier et deuxième espace)
                            Masque := Unbounded_Slice(Line_Content, Pos1, Pos2 - 1);
                            -- On récupère l'interface (après le deuxième espace)
			    while To_String(Line_Content) (Pos2) = ' ' loop
				Pos2 := Pos2 + 1;
			    end loop;

                            Interface_S := Unbounded_Slice(Line_Content, Pos2, Length(Line_Content));

			    -- Put(Integer'Image(Length(Interface_S)));

                            -- On applique le masque à la destination
                            Destination_Masquee := Ub_To_Ip(Destination);
                            Adresse_IP.Masquer_Adresse(Destination_Masquee, Ub_To_Ip(Masque));
			
                            -- On enregistre dans la table : clé = destination masquée, valeur = interface
                            Tab_Routage.Enregistrer(Table_Routage, Destination_Masquee, T_Route'(Ub_To_Ip(Masque), Interface_S));
			  
                        end if;
                    end if;
                end if;
            exception
                when End_Error => Null;
            end;
        end loop;

        Close(Entree_Table);
	
    end Construire_Table;

    -- Variables globales pour la recherche de la meilleure route
    Recherche_IP_Paquet : T_IP;           -- L'IP qu'on cherche à router
    Meilleure_Inter     : Unbounded_String; -- Le résultat trouvé
    Meilleur_Score      : Integer;          -- Le nombre de zéros du masque (plus il est petit, mieux c'est)

    -- Procédure appelée pour chaque ligne de la table de routage
    procedure Examiner_Route (Destination : in T_IP; Route : in T_Route) is
        Test_IP : T_IP;
        Score_Courant : Integer;
    begin
        Test_IP := Recherche_IP_Paquet;
        
        -- On applique le masque de cette route à notre paquet
        Adresse_IP.Masquer_Adresse(Test_IP, Route.Masque);

        -- Si (Paquet MASQUÉ) == Destination de la route
        if Ip_To_Ub(Test_IP) = Ip_To_Ub(Destination) then
            
            -- On calcule la précision du masque (nombre de zéros à la fin)
            -- Moins il y a de zéros, plus le masque est long (/24 mieux que /16)
            Score_Courant := Adresse_IP.Adresse_Zero_Bit(Route.Masque);

            if Score_Courant < Meilleur_Score then
                Meilleur_Score := Score_Courant;
                Meilleure_Inter := Route.Inter;
            end if;
        end if;
    end Examiner_Route;

    -- Instanciation du parcours
    procedure Parcourir_Table_Pour_Recherche is new Tab_Routage.Faire_Pour_Chaque (Traiter => Examiner_Route);


	function Interface_Sortie (Adresse : T_IP; Table_Routage : T_LCA) return Unbounded_String is
	begin
        -- 1. Initialisation de la recherche
        Recherche_IP_Paquet := Adresse;
        Meilleure_Inter := To_Unbounded_String("Erreur_Pas_De_Route"); -- Valeur par défaut
        Meilleur_Score := 33; -- Impossible d'avoir plus de 32 zéros, donc 33 est le "pire" score initial

        -- 2. On lance l'examen de toutes les routes
        Parcourir_Table_Pour_Recherche(Table_Routage);

        -- 3. On retourne ce qu'on a trouvé de mieux
        return Meilleure_Inter;

	end Interface_Sortie;


    procedure Traiter_Ligne_Paquets (Ligne_p : in Unbounded_String; Num_Ligne : in Integer; Sortie_Resultats : in out File_Type; Continuer : in out Boolean; Table_Routage : in T_LCA) is

    Inter : Unbounded_String;

    begin
	
        if To_String(Ligne_p) = "table" then
            Put ("table (ligne " & Integer'Image (Num_Ligne) & ")");
            New_Line;
            Afficher_Table (Table_Routage);
            New_Line;

        elsif To_String(Ligne_p) = "fin" then
            Put ("fin (ligne " & Integer'Image (Num_Ligne) & ")");
            New_Line;
            Continuer := false;

        else

            Inter := Interface_Sortie (Ub_To_Ip(Ligne_p), Table_Routage);
            Put (Sortie_resultats, Ligne_p & " " & Inter);
            New_Line (Sortie_resultats);

        end if;

    end Traiter_Ligne_Paquets;

    procedure Supprime_Dernier (Chaine : in out Unbounded_String) is

	Chaine_Str : constant String := To_String (Chaine);

    begin

	Chaine := To_Unbounded_String (Chaine_Str(1 .. Length(Chaine) - 1));

    end Supprime_Dernier;

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
	    -- Supprime_Dernier (Ligne_p);
            Traiter_Ligne_Paquets (Ligne_p, Num_Ligne, Sortie_Resultats, Continuer, Table_Routage);

        end loop;


        Close (Entree_paquets);
        Close (Sortie_resultats);

    end Traiter_Fichiers_Paquets;

end Routeur;

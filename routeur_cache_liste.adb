with Ada.Strings;               use Ada.Strings;        -- pour Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

package body Routeur_Cache_Liste is

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


    procedure Recuperer_Arguments (Table : out Unbounded_String;
                                   Paquets : out Unbounded_String;
                                   Resultats : out Unbounded_String;
                                   Taille : out Integer;
                                   Politique : out Unbounded_String;
                                   Statistiques : out Boolean) is
    begin

        Table := To_Unbounded_String("table.txt");
        Paquets :=  To_Unbounded_String("paquets.txt");
        Resultats := To_Unbounded_String ("resultats.txt");
        Taille := 10;
        Politique := To_Unbounded_String ("FIFO");
        Statistiques := true;

        for indice in 1 .. Argument_Count loop
            if Argument(indice) = "-t" then
                Table := To_Unbounded_String(Argument (indice + 1));
            elsif Argument(indice) = "-q" then
                Paquets := To_Unbounded_String(Argument (indice + 1));
            elsif Argument(indice) = "-r" then
                Resultats := To_Unbounded_String(Argument (indice + 1));
            elsif Argument(indice) = "-c" then
                Taille := Integer'Value(Argument (indice + 1));
            elsif Argument(indice) = "-p" then
                Politique := To_Unbounded_String (Argument (indice + 1));
            elsif Argument(indice) = "-S" or Argument(indice) = "-s" then
                Statistiques := Argument (indice) = "-s";
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
    Recherche_IP_Paquet : T_IP;             -- L'IP qu'on cherche à router
    Meilleure_Inter     : Unbounded_String; -- Le résultat trouvé
    Meilleur_Score      : Integer;          -- Le nombre de zéros du masque

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

            -- On calcule la précision du masque

            Score_Courant := Adresse_IP.Adresse_Zero_Bit(Route.Masque);

            if Score_Courant < Meilleur_Score then
                Meilleur_Score := Score_Courant;
                Meilleure_Inter := Route.Inter;
            end if;
        end if;
    end Examiner_Route;

    -- Instanciation du parcours
    procedure Parcourir_Table_Pour_Recherche is new Tab_Routage.Faire_Pour_Chaque (Traiter => Examiner_Route);
    function Interface_Sortie (Adresse : T_IP; Table_Routage : T_LCA; Cache : T_Cache_Liste) return Unbounded_String is
        Bonne_Interface_S : Unbounded_String;
        Interface_S : Unbounded_String;
    begin
        -- Initialisation de la recherche
        Recherche_IP_Paquet := Adresse;
        Meilleure_Inter := To_Unbounded_String("Erreur_Pas_De_Route"); -- Valeur par défaut
        Meilleur_Score := 33;

        -- On lance l'examen de toutes les routes
        Parcourir_Table_Pour_Recherche(Table_Routage);
        Bonne_Interface_S := Meilleure_Inter;

        Parcourir_Table_Pour_Recherche(Cache.Contenu);
        Interface_S := Meilleure_Inter;
        Ajouter (Cache, Adresse, Interface_S, Bonne_Interface_S);

        return Meilleure_Inter;
    end Interface_Sortie;


    procedure Traiter_Ligne_Paquets (Ligne_p : in Unbounded_String; Num_Ligne : in Integer; Sortie_Resultats : in out File_Type; Continuer : in out Boolean; Table_Routage : in T_LCA; Cache : in out T_Cache_Liste) is

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
        elsif To_String(Ligne_p) = "table" then
            Put ("nombre de paquets : " & Cache.Statistiques.Nombre_Routes);
	    New_Line;
            Put ("nombre de defauts : " & Cache.Statistiques.Nombre_Defauts);
	    New_Line;
            Put ("taux d'erreur     : " & Cache.Statistiques.Nombre_Defauts / Cache.Statistiques.Nombre_Routes);
	    New_Line;

        elsif To_String(Ligne_p) = "cache" then
            Afficher_Debug (Cache.Contenu);

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

    procedure Traiter_Fichiers_Paquets (Paquets : in Unbounded_String; Resultats : in Unbounded_String; Table_Routage : in out T_LCA; Taille : Integer) is

	package Cache is new Cache_Liste (Capacite => Taille);
    	use Cache;

        Entree_Paquets : File_Type;
        Sortie_Resultats : File_Type;
        Continuer : Boolean;
        Num_Ligne : Integer;
        Ligne_p : Unbounded_String;
        Cache : T_Cache;

    begin

        Open (Entree_Paquets, In_File, To_String (Paquets));
        Create (Sortie_Resultats, Out_File, To_String (Resultats));
        Creer (Cache);
        Continuer := true;

        while not (End_Of_File (Entree_Paquets)) and Continuer loop

            Num_Ligne :=  Integer (Line (Entree_Paquets));
            Ligne_p := Get_Line (Entree_Paquets);
            Supprime_Dernier (Ligne_p);
            Traiter_Ligne_Paquets (Ligne_p, Num_Ligne, Sortie_Resultats, Continuer, Table_Routage, Cache);

        end loop;

        Detruire (Table_Routage);
        Detruire (Cache);
        Close (Entree_Paquets);
        Close (Sortie_Resultats);

    end Traiter_Fichiers_Paquets;

end Routeur_Cache_Liste;
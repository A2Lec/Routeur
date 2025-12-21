with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed; 
with Ada.Strings;              use Ada.Strings;      
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with IP;                       use IP;

package body Routeur is

    -- Récupère les noms des fichiers passés en arguments en ligne de commande
    procedure Recuperer_Arguments (
        Table     : out Unbounded_String;
        Paquets   : out Unbounded_String;
        Resultats : out Unbounded_String
    ) is
    begin
       
        -- valeurs par défaut
        Table     := To_Unbounded_String("table.txt");
        Paquets   := To_Unbounded_String("paquets.txt");
        Resultats := To_Unbounded_String("resultats.txt");

       
        -- On parcourt tous les arguments et on cherche les valeurs -t, -q, -r
        for i in 1 .. Argument_Count loop
            if Argument(i) = "-t" and i < Argument_Count then
                -- Si -t trouvé, récupérer l'argument suivant comme fichier table
                Table := To_Unbounded_String(Argument(i + 1));

            elsif Argument(i) = "-q" and i < Argument_Count then
                -- même chose pour fichier paquets
                Paquets := To_Unbounded_String(Argument(i + 1));

            elsif Argument(i) = "-r" and i < Argument_Count then
                -- même chose pour fichier résultats
                Resultats := To_Unbounded_String(Argument(i + 1));
            end if;
        end loop;
    end Recuperer_Arguments;



    -- Pour construire la table de routage à partir d'un fichier
    -- On lit les lignes du fichier sous la forme : "destination masque interface"
    -- On masque chaque destination et on enregistre la destination et l'interface correspondante.
    procedure Construire_Table (
        Nom_Fichier : in Unbounded_String;
        La_Table    : out T_Table_Routage
    ) is
        Entree_Table : File_Type;
        Destination  : Unbounded_String;
        Masque       : Unbounded_String;
        Interface_S  : Unbounded_String;
        Line_Content : Unbounded_String;
        Pos1, Pos2   : Integer;
        Destination_Masquee : Unbounded_String;
    begin
        Open(Entree_Table, In_File, To_String(Nom_Fichier));

        Tab_Routage.Initialiser(La_Table);

        while not End_Of_File(Entree_Table) loop
            begin
                Line_Content := To_Unbounded_String(Get_Line(Entree_Table));
                Line_Content := To_Unbounded_String(Trim(To_String(Line_Content), Both));

                if Length(Line_Content) > 0 then
                    -- Format attendu : "destination masque interface" 
                    -- On va découper la ligne en fonction des espaces.
                    Pos1 := 1;
                    Pos2 := Index(To_String(Line_Content), " ");
                    if Pos2 > 0 then
                        -- On récupère la destination (avant le premier espace)
                        Destination := Unbounded_Slice(Line_Content, Pos1, Pos2 - 1);
                        Pos1 := Pos2 + 1;
                        Pos2 := Index(To_String(Line_Content), " ", Pos1);
                        if Pos2 > 0 then
                            -- On récupère le masque (entre premier et deuxième espace)
                            Masque := Unbounded_Slice(Line_Content, Pos1, Pos2 - 1);
                            -- On récupère l'interface (après le deuxième espace)
                            Interface_S := Unbounded_Slice(Line_Content, Pos2 + 1, Length(Line_Content));

                            -- On applique le masque à la destination
                            Destination_Masquee := Destination;
                            IP.Masquer_Adresse(Destination_Masquee, Masque);

                            -- On enregistre dans la table : clé = destination masquée, valeur = interface
                            Tab_Routage.Enregistrer(
                                La_Table,
                                Destination_Masquee,
                                Interface_S
                            );
                        end if;
                    end if;
                end if;
            exception
                when End_Error => null;
            end;
        end loop;

        Close(Entree_Table);
    end Construire_Table;


    -- Pour traiter une ligne de paquet lue dans le fichier d'entrée
    -- Trois cas possibles :
    --   1. Ligne "table" : affiche la table de routage actuelle
    --   2. Ligne "fin" : arrête le traitement (Continuer := False)
    --   3. Autre ligne : on trouve la route optimale pour l'adresse IP donnée
    procedure Traiter_Ligne (
        Ligne_P          : in Unbounded_String;
        Num_Ligne        : in Integer;
        Sortie_Resultats : in out File_Type;
        Continuer        : in out Boolean;
        La_Table         : in T_Table_Routage
    ) is
        S_Ligne : String := To_String(Ligne_P);
        Cle_recherche : Unbounded_String;
        Pipe_Pos : Integer;

        IP_precedent : Unbounded_String;
        Trouve : Boolean := False;

        -- Pour afficher une clé lors du diagnostic de la table
        procedure Afficher_Cle (Cle : in Unbounded_String) is
        begin
            Put(Sortie_Resultats, To_String(Cle));
        end Afficher_Cle;

        -- Pour afficher une valeur lors du diagnostic de la table
        procedure Afficher_Donnee (Valeur : in Unbounded_String) is
        begin
            Put(Sortie_Resultats, " -> " & To_String(Valeur));
        end Afficher_Donnee;

        -- Pour afficher tout le contenu de la table
        procedure Afficher_Tout is new Tab_Routage.Afficher_Diagnostic(Afficher_Cle, Afficher_Donnee);
        
        Interface_Trouvee : Unbounded_String := To_Unbounded_String("?");  -- Valeur par défaut si pas trouvée
        IP_Masquee        : Unbounded_String;  -- Adresse IP qui sera progressivement masquée

    begin
        S_Ligne := Trim(S_Ligne, Both);

        if S_Ligne = "table" then
            -- On affiche la table de routage actuelle
            Put(Sortie_Resultats, "table (ligne " & Integer'Image(Num_Ligne) & ")");
            New_Line(Sortie_Resultats);

            Afficher_Tout(La_Table);
            New_Line(Sortie_Resultats);

        elsif S_Ligne = "fin" then
            -- On arrête le traitement des paquets
            Continuer := False;
            Put(Sortie_Resultats, "fin (ligne " & Integer'Image(Num_Ligne) & ")");
            New_Line(Sortie_Resultats);

        elsif S_Ligne'Length > 0 then
            -- Pour traiter une adresse IP
            -- On initialise la clé de recherche avec l'adresse IP complète
            Cle_recherche := To_Unbounded_String(S_Ligne);            

            IP_Masquee := Cle_recherche;

            -- On cherche l'adresse IP en masquant progressivement un bit à la fois
            begin
                while not Trouve loop
                    if Tab_Routage.Cle_Presente(La_Table, IP_Masquee) then
                        -- On récupère la bonne interface
                        Interface_Trouvee := Tab_Routage.La_Valeur(La_Table, IP_Masquee);
                        Trouve := True;
                    else
                        -- On masque un bit supplémentaire 
                        IP_precedent := IP_Masquee;
                        IP.Masquer_Bit(IP_Masquee);
                        
                        -- Si l'adresse n'a pas changé, on sort de la boucle
                        if IP_Masquee = IP_precedent then
                            Trouve := True;
                        end if;
                    end if;
                end loop;
            end;

            -- On écrit le résultat : adresse IP -> interface trouvée
            Put(Sortie_Resultats, S_Ligne & " -> " & To_String(Interface_Trouvee));
            New_Line(Sortie_Resultats);
        end if;
    end Traiter_Ligne;


    -- Pour traiter le fichier des paquets et écrire les résultats du routage
    -- Lit chaque ligne du fichier paquets et l'envoie à Traiter_Ligne pour la résoudre
    -- S'arrête à  la fin du fichier ou si on lit "fin" dans le fichier
    procedure Traiter_Fichiers_Paquets (
        Fichier_Paquets   : in Unbounded_String;
        Fichier_Resultats : in Unbounded_String;
        La_Table          : in T_Table_Routage
    ) is
        Entree_Paquets   : File_Type;
        Sortie_Resultats : File_Type;
        Continuer        : Boolean := True; -- booleen pour continuer le traitement
        Ligne_P          : Unbounded_String;  
        Num_Ligne        : Integer;  
    begin
        Open(Entree_Paquets, In_File, To_String(Fichier_Paquets));
        Create(Sortie_Resultats, Out_File, To_String(Fichier_Resultats));

        
        while not End_Of_File(Entree_Paquets) and Continuer loop
            -- Récupérer le numéro de la ligne actuelle
            Num_Ligne := Integer(Line(Entree_Paquets)); 
            
            -- Lire la ligne du fichier
            Ligne_P := To_Unbounded_String(Get_Line(Entree_Paquets)); 
            
            -- Traiter la ligne et écrire le résultat dans le fichier de sortie
            Traiter_Ligne(Ligne_P, Num_Ligne, Sortie_Resultats, Continuer, La_Table);
        end loop;

        Close(Entree_Paquets);
        Close(Sortie_Resultats);
    end Traiter_Fichiers_Paquets;

end Routeur;
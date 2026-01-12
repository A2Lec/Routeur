with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Adresse_IP;                use Adresse_IP;
with LCA;
with Cache_Trie;

-- Programme principal routeur_LA : Routeur avec Liste chaînée + Arbre préfixe (cache)
procedure Routeur_LA is

    -- Type pour la table de routage
    type T_Route is record
        Masque : T_IP;
        Inter  : Unbounded_String;
    end record;

    package Tab_Routage is new LCA (T_Cle => T_IP, T_Valeur => T_Route);
    use Tab_Routage;

    -- Variables globales
    Table_Routage : T_LCA;
    Cache : Cache_Trie.T_Cache;
    -- Nb_Routes_Total : Integer := 0;
    
    -- Paramètres de configuration
    Nom_Fichier_Table     : Unbounded_String := To_Unbounded_String("table.txt");
    Nom_Fichier_Paquets   : Unbounded_String := To_Unbounded_String("paquets.txt");
    Nom_Fichier_Resultats : Unbounded_String := To_Unbounded_String("resultats.txt");
    Taille_Cache          : Integer := 10;
    Politique             : Cache_Trie.T_Politique := Cache_Trie.FIFO;
    Afficher_Stats        : Boolean := True;

    ---------------------------------------------------------------------------
    -- Convertir une chaîne en politique
    ---------------------------------------------------------------------------
    function String_Vers_Politique(S : String) return Cache_Trie.T_Politique is
    begin
        if S = "FIFO" or S = "fifo" then
            return Cache_Trie.FIFO;
        elsif S = "LRU" or S = "lru" then
            return Cache_Trie.LRU;
        elsif S = "LFU" then
            return Cache_Trie.LFU;
        else
            Put_Line("Politique inconnue '" & S & "', utilisation de FIFO par défaut");
            return Cache_Trie.FIFO;
        end if;
    end String_Vers_Politique;

    ---------------------------------------------------------------------------
    -- Parser les arguments de la ligne de commande
    ---------------------------------------------------------------------------
    procedure Recuperer_Arguments is
    begin
        for I in 1 .. Argument_Count loop
            if Argument(I) = "-t" and I < Argument_Count then
                Nom_Fichier_Table := To_Unbounded_String(Argument(I + 1));
                
            elsif Argument(I) = "-q" and I < Argument_Count then
                Nom_Fichier_Paquets := To_Unbounded_String(Argument(I + 1));
                
            elsif Argument(I) = "-r" and I < Argument_Count then
                Nom_Fichier_Resultats := To_Unbounded_String(Argument(I + 1));
                
            elsif Argument(I) = "-c" and I < Argument_Count then
                Taille_Cache := Integer'Value(Argument(I + 1));
                
            elsif Argument(I) = "-p" and I < Argument_Count then
                Politique := String_Vers_Politique(Argument(I + 1));
                
            elsif Argument(I) = "-s" then
                Afficher_Stats := True;
                
            elsif Argument(I) = "-S" then
                Afficher_Stats := False;
            end if;
        end loop;
    end Recuperer_Arguments;

    ---------------------------------------------------------------------------
    -- Construire la table de routage depuis le fichier
    ---------------------------------------------------------------------------
    procedure Construire_Table is
        Fichier : File_Type;
        Ligne : Unbounded_String;
        Destination : T_IP;
        Route : T_Route;
        Debut, Fin : Integer;
        -- compteur_lignes : Integer := 0;
    begin
        Tab_Routage.Initialiser(Table_Routage);
        
        Open(Fichier, In_File, To_String(Nom_Fichier_Table));
        
        while not End_Of_File(Fichier) loop
            Ligne := Get_Line(Fichier);
            Ligne := Trim(Ligne, Ada.Strings.Both);
            
            if Length(Ligne) > 0 then
                -- Parser la ligne : Destination Masque Interface
                Debut := 1;
                Fin := Index(Ligne, " ", Debut);
                Destination := Ub_To_Ip(Unbounded_Slice(Ligne, Debut, Fin - 1));
                
                Debut := Fin + 1;
                while Debut <= Length(Ligne) and then Element(Ligne, Debut) = ' ' loop
                    Debut := Debut + 1;
                end loop;
                
                Fin := Index(Ligne, " ", Debut);
                Route.Masque := Ub_To_Ip(Unbounded_Slice(Ligne, Debut, Fin - 1));
                
                Debut := Fin + 1;
                while Debut <= Length(Ligne) and then Element(Ligne, Debut) = ' ' loop
                    Debut := Debut + 1;
                end loop;
                
                Route.Inter := Unbounded_Slice(Ligne, Debut, Length(Ligne));
                
                Tab_Routage.Enregistrer(Table_Routage, Destination, Route);
            end if;
        end loop;
        
        Close(Fichier);
    end Construire_Table;

    ---------------------------------------------------------------------------
    -- Afficher une route de la table
    ---------------------------------------------------------------------------
    procedure Afficher_Une_Route(Destination : in T_IP; Route : in T_Route) is
    begin
        Put(Ip_To_Ub(Destination));
        Put(" ");
        Put(Ip_To_Ub(Route.Masque));
        Put(" ");
        Put(Route.Inter);
        New_Line;
    end Afficher_Une_Route;

    procedure Afficher_Table is new Tab_Routage.Faire_Pour_Chaque (Traiter => Afficher_Une_Route);

    ---------------------------------------------------------------------------
    -- Rechercher une route dans la table (avec masque le plus long)
    ---------------------------------------------------------------------------
    function Rechercher_Route_Table(Adresse : T_IP) return T_Route is
        Route_Trouvee : T_Route;
        Trouve : Boolean := False;
        Nb_Bits_Zero_Min : Integer := 33;  -- Plus petit = masque plus long
        
        procedure Tester_Route(Destination : in T_IP; Route : in T_Route) is
            Nb_Bits_Zero : Integer;
            Adresse_Test : T_IP;  -- Variable locale pour chaque test
        begin
            Adresse_Test := Adresse;
            Masquer_Adresse(Adresse_Test, Route.Masque);
            
            if To_String(Ip_To_Ub(Adresse_Test)) = To_String(Ip_To_Ub(Destination)) then
                -- Cette route correspond, vérifier si le masque est plus long
                -- Plus le masque est long, moins il y a de bits à 0
                Nb_Bits_Zero := Adresse_Zero_Bit(Route.Masque);
                if Nb_Bits_Zero < Nb_Bits_Zero_Min then
                    Nb_Bits_Zero_Min := Nb_Bits_Zero;
                    Route_Trouvee := Route;
                    Trouve := True;
                end if;
            end if;
        end Tester_Route;
        
        procedure Chercher is new Tab_Routage.Faire_Pour_Chaque(Traiter => Tester_Route);
        
    begin
        Chercher(Table_Routage);
        
        if not Trouve then
            -- Route par défaut
            Route_Trouvee.Inter := To_Unbounded_String("???");
        end if;
        
        return Route_Trouvee;
    end Rechercher_Route_Table;

    ---------------------------------------------------------------------------
    -- Traiter les paquets
    ---------------------------------------------------------------------------
    procedure Traiter_Paquets is
        Fichier_Paquets : File_Type;
        Fichier_Resultats : File_Type;
        Ligne : Unbounded_String;
        Adresse : T_IP;
        Interface_Sortie : Unbounded_String;
        Route : T_Route;
        Num_Ligne : Integer := 0;
        Adresse_Masquee : T_IP;
        Longueur_Prefixe : Integer;
    begin
        Open(Fichier_Paquets, In_File, To_String(Nom_Fichier_Paquets));
        Create(Fichier_Resultats, Out_File, To_String(Nom_Fichier_Resultats));
        
        while not End_Of_File(Fichier_Paquets) loop
            Ligne := Get_Line(Fichier_Paquets);
            Ligne := Trim(Ligne, Ada.Strings.Both);
            Num_Ligne := Num_Ligne + 1;
            
            if Length(Ligne) > 0 then
                -- Vérifier si c'est une commande
                if To_String(Ligne) = "table" then
                    New_Line;
                    Put("table (ligne ");
                    Put(Num_Ligne, 0);
                    Put_Line(")");
                    Afficher_Table(Table_Routage);
                    
                elsif To_String(Ligne) = "cache" then
                    New_Line;
                    Put("cache (ligne ");
                    Put(Num_Ligne, 0);
                    Put_Line(")");
                    Cache_Trie.Afficher(Cache);
                    
                elsif To_String(Ligne) = "stat" then
                    New_Line;
                    Put("stat (ligne ");
                    Put(Num_Ligne, 0);
                    Put_Line(")");
                    Cache_Trie.Afficher_Stats(Cache);
                    
                elsif To_String(Ligne) = "fin" then
                    New_Line;
                    Put("fin (ligne ");
                    Put(Num_Ligne, 0);
                    Put_Line(")");
                    exit;
                    
                else
                    -- C'est une adresse IP à router
                    Adresse := Ub_To_Ip(Ligne);
                    
                    -- Chercher dans le cache d'abord
                    if not Cache_Trie.Rechercher(Cache, Adresse, Interface_Sortie) then
                        -- Défaut de cache : chercher dans la table
                        Route := Rechercher_Route_Table(Adresse);
                        Interface_Sortie := Route.Inter;
                        
                        -- Calculer le masque à insérer dans le cache
                        -- On utilise le masque le plus long qui discrimine cette route
                        Adresse_Masquee := Adresse;
                        Masquer_Adresse(Adresse_Masquee, Route.Masque);
                        Longueur_Prefixe := 32 - Adresse_Zero_Bit(Route.Masque);
                        
                        -- Ajouter au cache (si taille > 0)
                        if Taille_Cache > 0 then
                            Cache_Trie.Ajouter(Cache, Adresse_Masquee, Longueur_Prefixe, Interface_Sortie);
                        end if;
                    end if;
                    
                    -- Écrire le résultat
                    Put(Fichier_Resultats, Ip_To_Ub(Adresse));
                    Put(Fichier_Resultats, " ");
                    Put(Fichier_Resultats, Interface_Sortie);
                    New_Line(Fichier_Resultats);
                end if;
            end if;
        end loop;
        
        Close(Fichier_Paquets);
        Close(Fichier_Resultats);
    end Traiter_Paquets;

begin
    -- Récupérer les arguments
    Recuperer_Arguments;
    
    -- Afficher la configuration
    Put_Line("=== Routeur LA (Liste + Arbre) ===");
    Put_Line("Table de routage  : " & To_String(Nom_Fichier_Table));
    Put_Line("Fichier paquets   : " & To_String(Nom_Fichier_Paquets));
    Put_Line("Fichier resultats : " & To_String(Nom_Fichier_Resultats));
    Put("Taille du cache   : ");
    Put(Taille_Cache, 0);
    New_Line;
    Put("Politique         : ");
    case Politique is
        when Cache_Trie.FIFO => Put_Line("FIFO");
        when Cache_Trie.LRU => Put_Line("LRU");
        when Cache_Trie.LFU => Put_Line("LFU");
    end case;
    Put_Line("===================================");
    New_Line;
    
    -- Construire la table de routage
    Construire_Table;
    Put_Line("Table de routage chargee.");
    
    -- Initialiser le cache
    Cache_Trie.Initialiser(Cache, Taille_Cache, Politique);
    Put_Line("Cache initialise.");
    New_Line;
    
    -- Traiter les paquets
    Traiter_Paquets;
    
    -- Afficher les statistiques finales
    if Afficher_Stats then
        New_Line;
        Cache_Trie.Afficher_Stats(Cache);
    end if;
    
    Put_Line("Traitement termine.");
    
end Routeur_LA;

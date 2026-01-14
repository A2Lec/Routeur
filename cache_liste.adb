with Ada.Text_IO;               use Ada.Text_IO;

package body Cache_Liste is

    procedure Creer (Cache : out T_Cache_Liste; Politique : in T_Politique; Taille : in Integer) is
  
    begin
        Initialiser (Cache.Contenu);
        Cache.Politique := Politique;
        Cache.Ordre_Politique.Taille := 0;
        Cache.Statistiques.Nombre_Routes := 0;
        Cache.Statistiques.Nombre_Defauts := 0;
	Cache.Capacite := Taille;
    end Creer;

    procedure Supprimer (Cache : in out T_Cache_Liste) is
        Destination : T_Destination;
    begin
        Destination := Cache.Ordre_Politique.Adresses(Cache.Ordre_Politique.Taille);
        Supprimer (Cache.Contenu, Destination.Adresse);
        Cache.Ordre_Politique.Taille := Cache.Ordre_Politique.Taille - 1;
    end Supprimer;


    procedure Detruire (Cache : in out T_Cache_Liste) is
    begin
        Detruire (Cache.Contenu);
    end Detruire;

    procedure Ajouter (Cache : in out T_Cache_Liste; Adresse : in T_IP; Interface_S : in Unbounded_String; Ajout : in Boolean) is
        Indice : Integer;
        Occurrence : Integer;
        Route : T_Route;

    begin

        Cache.Statistiques.Nombre_Routes := Cache.Statistiques.Nombre_Routes + 1;
        if Ajout then
            Cache.Statistiques.Nombre_Defauts := Cache.Statistiques.Nombre_Defauts + 1;
        else
	    Null;
        end if;

        if Cache.Ordre_Politique.Taille = Cache.Capacite then
            Supprimer (Cache);
        else
            Null;
        end if;

        if Ajout then
            Route.Inter := Interface_S;
            Route.Masque := Adresse_IP.Ub_To_Ip(To_Unbounded_String("255.255.255.255"));
            Enregistrer(Cache.Contenu, Adresse, Route);
        else
            Null;
        end if;

        case Cache.Politique is

            when FIFO =>
                if Ajout then
                    Indice := Cache.Capacite;
                    while Indice > 1 loop
                        Cache.Ordre_Politique.Adresses (Indice) := Cache.Ordre_Politique.Adresses (Indice - 1);
                        Indice := Indice - 1;
                    end loop;
                    Cache.Ordre_Politique.Adresses(1).Adresse := Adresse;
                    Cache.Ordre_Politique.Taille := Cache.Ordre_Politique.Taille + 1;
                else
                    Null;
                end if;

            when LRU =>

                if Ajout then
                    Indice := Cache.Ordre_Politique.Taille;
                else
                    Indice := 1;
                    while Indice + 1 < Cache.Capacite and then Cache.Ordre_Politique.Adresses(Indice + 1).Adresse /= Adresse loop
                        Indice := Indice + 1;
		    end loop;
                end if;

                while Indice > 0 loop
                    Cache.Ordre_Politique.Adresses (Indice + 1) := Cache.Ordre_Politique.Adresses (Indice);
                    Indice := Indice - 1;
                end loop;

                Cache.Ordre_Politique.Adresses(1).Adresse := Adresse;

                if Ajout then
                    Cache.Ordre_Politique.Taille := Cache.Ordre_Politique.Taille + 1;
                else
                    Null;
                end if;

            when LFU =>

                if Ajout then
                    Cache.Ordre_Politique.Adresses (Cache.Ordre_Politique.Taille + 1).Adresse := Adresse;
                    Cache.Ordre_Politique.Adresses (Cache.Ordre_Politique.Taille + 1).Occurrences := 1;
                    Cache.Ordre_Politique.Taille := Cache.Ordre_Politique.Taille + 1;
                else
                    Indice := 1;
                    while To_String(Ip_To_Ub(Cache.Ordre_Politique.Adresses(Indice).Adresse)) /= To_String(Ip_To_Ub(Adresse)) loop
                        Indice := Indice + 1;
                    end loop;
                    Cache.Ordre_Politique.Adresses(Indice).Occurrences := Cache.Ordre_Politique.Adresses(Indice).Occurrences + 1;
                    Occurrence := Cache.Ordre_Politique.Adresses(Indice).Occurrences;
                    while Indice > 1 and then Occurrence > Cache.Ordre_Politique.Adresses(Indice - 1).Occurrences loop
                        Cache.Ordre_Politique.Adresses(Indice) := Cache.Ordre_Politique.Adresses(Indice - 1);
                        Indice := Indice - 1;
                    end loop;
                    Cache.Ordre_Politique.Adresses(Indice).Adresse := Adresse;
                    Cache.Ordre_Politique.Adresses(Indice).Occurrences := Occurrence;
                end if;
        end case;

    end Ajouter;
    
    
    function Stats (Cache : T_Cache_Liste) return T_Statistiques is
    
    begin
        return Cache.Statistiques;
    end Stats;

    
    
    procedure Afficher_Table_Cache (Cache : in T_Cache_Liste) is
    begin
        Afficher_Table(Cache.Contenu);
    end Afficher_Table_Cache;
    
    procedure Afficher_Une_Route (Destination : in T_IP; Route : in T_Route) is
    begin
        Put(To_String(Ip_To_Ub(Destination)));
        Put(" ");
        Put(To_String(Ip_To_Ub(Route.Masque)));
        Put(" ");
        Put(To_String(Route.Inter));
        New_Line;
    end Afficher_Une_Route;
    
    procedure Afficher_Table (Cache : in T_Cache_Liste) is
    begin
	Afficher_Table (Cache.Contenu);
    end Afficher_Table;

    
    Recherche_IP_Paquet : T_IP;             -- L'IP qu'on cherche à router
    Meilleure_Inter     : Unbounded_String; -- Le résultat trouvé
    Meilleur_Score      : Integer;          -- Le nombre de zéros du masque

    
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


    procedure Interface_Sortie (Adresse : in T_IP; Table_Routage : in Tab_Routage.T_LCA; Cache : in out T_Cache_Liste; Inter : out Unbounded_String) is
	Ajout : Boolean;
    begin
	Recherche_IP_Paquet := Adresse;
        Meilleure_Inter := To_Unbounded_String("Erreur_Pas_De_Route"); -- Valeur par défaut
        Meilleur_Score := 33;
	Ajout := not Cle_Presente(Cache.Contenu, Adresse);
	if Ajout then
	    Parcourir_Table_Pour_Recherche(Table_Routage);
        else
	    Parcourir_Table_Pour_Recherche(Cache.Contenu);
	end if;

	Inter := Meilleure_Inter;
        Ajouter (Cache, Adresse, Meilleure_Inter, Ajout);

    end Interface_Sortie;
    
end Cache_Liste;

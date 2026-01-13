

package body Cache_Liste is

    procedure Creer (Cache : out T_Cache_Liste; Politique : in T_Politique) is
  
    begin
        Initialiser (Cache.Contenu);
        Cache.Politique := Politique;
        Cache.Ordre_Politique.Taille := 0;
        Cache.Statistiques.Nombre_Routes := 0.0;
        Cache.Statistiques.Nombre_Defauts := 0.0;
    end Creer;

    procedure Supprimer (Cache : in out T_Cache_Liste) is
        Destination : T_Destination;
    begin

        Destination := Cache.Ordre_Politique.Adresses(Cache.Ordre_Politique.Taille - 1);
        Supprimer (Cache.Contenu, Destination.Adresse);
        Cache.Ordre_Politique.Taille := Cache.Ordre_Politique.Taille - 1;
    end Supprimer;

    procedure Detruire (Cache : in out T_Cache_Liste) is

    begin
        Detruire (Cache.Contenu);
    end Detruire;

    procedure Ajouter (Cache : in out T_Cache_Liste; Adresse : in T_IP; Interface_S : Unbounded_String; Bonne_Interface_S : Unbounded_String) is
        Ajout : Boolean;
        Indice : Integer;
        Occurrence : Integer;
        Masque : T_IP := Adresse_IP.Ub_To_Ip(To_Unbounded_String("255.255.255.255"));
        Nb_Zero : Integer;
        Route : T_Route;

    begin

        Cache.Statistiques.Nombre_Routes := Cache.Statistiques.Nombre_Routes + 1.0;
        if Interface_S /= Bonne_Interface_S then
            Cache.Statistiques.Nombre_Defauts := Cache.Statistiques.Nombre_Defauts + 1.0;
        else
	    null;
        end if;

        if Taille(Cache.Contenu) = Cache.Capacite then
            Supprimer (Cache);
        else
            null;
        end if;

        Ajout := not Cle_Presente (Cache.Contenu, Adresse);

        if Ajout then
            Nb_Zero := Adresse_Zero_Bit (Adresse);
            for i in 1..Nb_Zero loop
                Adresse_IP.Masquer_Bit(Masque);
            end loop;
            Route.Inter := Interface_S;
            Route.Masque := Masque;
            Enregistrer(Cache.Contenu, Adresse, Route);
        else
            Null;
        end if;

        case Cache.Politique is

            when FIFO =>
                if Ajout then
                    Indice := Cache.Ordre_Politique.Taille - 1;
                    while Indice > 0 loop
                        Cache.Ordre_Politique.Adresses (Indice + 1) := Cache.Ordre_Politique.Adresses (Indice);
                        Indice := Indice - 1;
                    end loop;
                    Cache.Ordre_Politique.Adresses(1).Adresse := Adresse;
                    Cache.Ordre_Politique.Taille := Cache.Ordre_Politique.Taille + 1;
                else
                    Null;
                end if;

            when LRU =>

                if Ajout then
                    Indice := Cache.Ordre_Politique.Taille - 1;
                else
                    Indice := 1;
                    while Cache.Ordre_Politique.Adresses(Indice + 1).Adresse /= Adresse loop
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
                    Cache.Ordre_Politique.Adresses (Cache.Ordre_Politique.Taille).Adresse := Adresse;
                    Cache.Ordre_Politique.Adresses (Cache.Ordre_Politique.Taille).Occurrences := 1;
                    Cache.Ordre_Politique.Taille := Cache.Ordre_Politique.Taille + 1;
                else
                    Indice := 1;
                    while Cache.Ordre_Politique.Adresses(Indice).Adresse /= Adresse loop
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
    
    
end Cache_Liste;

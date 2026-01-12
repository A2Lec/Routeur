with LCA;

package body Cache_Liste is

    procedure Creer (Cache : out T_Cache_Liste; Politique : in T_Politique) is
  
    begin
        Initialiser (Cache.Contenu);
        Cache.Politique := Politique;
        Cache.Ordre_Politique.Taille := 0;
        Cache.Statstiques.Nombre_Routes := 0.0;
        Cache.Statstiques.Nombre_Defauts := 0.0;
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

    begin

        Cache.Statstiques.Nombre_Routes := Cache.Statstiques.Nombre_Routes + 1.0;
        if Interface_S /= Bonne_Interface_S then
            Cache.Statstiques.Nombre_Defauts := Cache.Statstiques.Nombre_Defauts + 1.0;
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
            Enregistrer(Cache.Contenu, Adresse, Interface_S);
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

end Cache_Liste;

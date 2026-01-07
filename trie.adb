with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Trie;



package body Trie is

	function EstVide(trie : in T_Trie) return Boolean is
	begin
		return trie = null;
	end EstVide;

        function EstFeuille(trie : in T_Trie) return Boolean is
		est_feuille : Boolean := True;
	begin
		if EstVide(trie) then
			return False;
		else
			for i in T_Enum_IP loop
				if not EstVide(trie.all.Noeuds(i)) then
					est_feuille := False;
				end if;
			end loop;
			return est_feuille;
		end if;
	end EstFeuille;

	function Taille(trie : in T_Trie) return Integer is
		Somme : Integer := 0;
	begin
		if EstVide(trie) then
			return 0;
		else
			for i in T_Enum_IP loop
				Somme := Somme + Taille(trie.all.Noeuds(i)); 
			end loop;

			if Somme = 0 then
				return 1;
			else
				return Somme;
			end if;
		end if;
	end Taille;

	procedure AjouterAdresse(trie : out T_Trie; adresse : in T_IP) is
		adrStr : Unbounded_String;
	begin

		if EstVide(trie) then
			trie := new T_Cellule_Trie;
			trie.all.Valeur := Element(adresse,1);
			for i in T_Enum_IP loop
				trie.all.Noeuds(i) := null;
			end loop;
		
			if Length(adresse) > 1 then
 				adrStr := To_Unbounded_String(Slice(Ip_To_Ub(adresse), 2, Length(adresse)));
				
				if Element(adresse, 2) = '.' then
					AjouterAdresse(trie.all.Noeuds(T_Enum_IP(11)), Ub_To_Ip(adrStr));
				else
					AjouterAdresse(trie.all.Noeuds(T_Enum_IP'Value((1 => Element(adresse, 2)))), Ub_To_Ip(adrStr));
				end if;
			end if;
		else
			if Length(adresse) > 1 then
				adrStr := To_Unbounded_String(Slice(Ip_To_Ub(adresse), 2, Length(adresse)));
				
				if Element(adresse, 2) = '.' then
					AjouterAdresse(trie.all.Noeuds(T_Enum_IP(11)), Ub_To_Ip(adrStr));
				else
					AjouterAdresse(trie.all.Noeuds(T_Enum_IP'Value((1 => Element(adresse, 2)))), Ub_To_Ip(adrStr));
				end if;

			end if;
		end if;
	end AjouterAdresse;


	procedure SupprimerAdresse(trie : in out T_Trie; adresse : T_IP) is
	begin
		null;
	end SupprimerAdresse;


	function AdresseExiste(trie : in T_Trie; adresse: in T_IP) return Boolean is
		adrStr : Unbounded_String;
	begin
		if EstVide(trie) then
                        if Length(adresse) > 0 then
                        	return False;
			else
				return True;
			end if;
                else
                        if not (Element(adresse, 1) = trie.all.Valeur) then	
				return False;
			end if;
			
			if Length(adresse) > 1 then
                                adrStr := To_Unbounded_String(Slice(Ip_To_Ub(adresse), 2, Length(adresse)));

                                if Element(adresse, 2) = '.' then
                                        return AdresseExiste(trie.all.Noeuds(T_Enum_IP(11)), Ub_To_Ip(adrStr));
                                else
                                        return AdresseExiste(trie.all.Noeuds(T_Enum_IP'Value((1 => Element(adresse, 2)))), Ub_To_Ip(adrStr));
                                end if;
			else
				if Element(adresse, 1) = trie.all.Valeur then
                                	return True;
                       		end if;
                        end if;
                end if;
	end AdresseExiste;

	procedure Affiche_debug(trie : in T_Trie) is
	begin
		if EstVide(trie) then
			return;
		else
			Put(trie.all.Valeur);
			if EstFeuille(trie) then
				New_Line;
				return;
			
			end if;
			
			for i in T_Enum_IP loop

				if not EstVide(trie.all.Noeuds(i)) then
					Affiche_debug(trie.all.Noeuds(i));
				end if;
			end loop;
		end if;

	end Affiche_debug;

end Trie;



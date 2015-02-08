#include <stack>
#include <algorithm>  
#include "eqclass.h"
#include "treeminer.h"

#define LOCAL_BRANCHIT -1

bool Eqnode::find_in_tlist(vector<int> &par, int lit, 
                           ival &it, int stpos)
{
   //idlist::iterator ii;
   //for (ii = &tlist[stpos]; ii != tlist.end(); ++ii){
   idnode *ii;
   for (int i = stpos; i < tlist.size(); ++i){
      ii = &tlist[stpos];
      
      if (equal(par.begin(), par.end(), (*ii).parscope.begin())){
         if ((lit == (*ii).parscope.back()) &&
             (ival::compare((*ii).itscope, it) == equals)) return true;
      }
   }
   return false;
}

ostream & operator<<(ostream& ostr, Eqnode& eqn){
   int *fidx = Dbase_Ctrl_Blk::FreqIdx;
   //ostr << fidx[eqn.val] << " (" << eqn.pos << ") - " << eqn.sup << endl;
   //ostr << fidx[eqn.val] << " - " << eqn.sup << " , " << eqn.isup <<
   //endl;
   if (mine_induced) ostr << " - " << eqn.isup << endl;
   else ostr << " - " << eqn.sup << endl;
   if (output_idlist) ostr << eqn.tlist;
   return ostr;
}

Eqclass::~Eqclass()
{
   //list<Eqnode *>::iterator ni = _nodelist.begin();
   for_each(nlist().begin(), nlist().end(), delnode<Eqnode *>());
}
 
void Eqclass::add_node(int val, int pos, int sup)
{
   Eqnode *eqn = new Eqnode(val,pos,sup);
   _nodelist.push_back(eqn);
}

void Eqclass::add_node(Eqnode *eqn)
{
   _nodelist.push_back(eqn);
}


int Eqclass::item(int n) {
   int i,k;
   //cout << "in item: " << n << " " << _prefix.size() << endl;
   for (i=0,k=0; i < _prefix.size(); i++){
      if (_prefix[i] != BranchIt){
         if (k == n) return _prefix[i];
         k++;
      }
   }
   return -1;
}

//return the scope of the prefix
int Eqclass::get_scope(int pos, int &scope){
  int scnt=0;
  for (int i=0; i < _prefix.size(); i++){
    if (_prefix[i] == BranchIt) scnt--;
    else scnt++;
    if (i == pos) scope = scnt;
  }
  return scnt;
}
 
void Eqclass::set_prefix(vector<int> &pref, Eqnode &node)
{
   _prefix = pref;
   
   int scope, scnt;
   scnt = get_scope(node.pos, scope); //what is the scope of node.pos

   while(scnt > scope){ 
     _prefix.push_back(BranchIt);
     scnt--;
   }
   _prefix.push_back(node.val);
}

ostream & Eqclass::print_prefix(ostream& fout)
{
   for (int i=0; i < _prefix.size(); i++){
      if (_prefix[i] == BranchIt) fout << LOCAL_BRANCHIT << " ";
      else fout << Dbase_Ctrl_Blk::FreqIdx[_prefix[i]] << " ";
   }
   return fout;
}

//print with items remapped to their original value
void Eqclass::print(bool check_isup){
  list<Eqnode *>::iterator ni = _nodelist.begin();
  int st, en;
  for (; ni != _nodelist.end(); ni++){
     if (check_isup && (*ni)->isup < MINSUPPORT) continue;
     
     print_prefix();
     en = get_scope((*ni)->pos, st);
     while (en > st){
        st++;
        cout << LOCAL_BRANCHIT << " ";
     }
     //cout << Dbase_Ctrl_Blk::FreqIdx[(*ni)->val] << " - " 
     //      << (*ni)->sup << " , " << (*ni)->isup << endl;    
     cout << *(*ni);
  }
} 

ostream& operator << (ostream& fout, Eqclass& eq)
{
  list<Eqnode *>::iterator ni = eq._nodelist.begin();
  int st, en;
  for (; ni != eq._nodelist.end(); ni++){
     eq.print_prefix(fout);
     en = eq.get_scope((*ni)->pos, st);
     while (en > st){
        st++;
        fout << LOCAL_BRANCHIT << " ";
     }
     fout << *(*ni);
  }
  return fout;
}
 

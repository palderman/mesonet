#include <Rcpp.h>
using namespace Rcpp;

int round_wdir(double& wdir){
  return std::floor(std::fmod((wdir + 360./32.), 360) / 360 * 16);
}

void reset_vec(IntegerVector& v){
  std::fill(v.begin(), v.end(), 0);
}

template<int RTYPE>
Vector<RTYPE> allocate_vector(Vector<RTYPE> x, int v_len){
  return Vector<RTYPE>(v_len);
}

RObject new_column(RObject x, int v_len){
  RCPP_RETURN_VECTOR(allocate_vector, x, v_len);
}

template<int RTYPE>
void copy_column(Vector<RTYPE>& x, Vector<RTYPE>& x_out, IntegerVector& g_ind){
  int i = 0;
  int j = 0;
  bool first = 1;
  while(j < x_out.size()){
    while(i < x.size() && g_ind[i] == (j + 1)){
      // x_out was initialized with NA
      if(first){
        if(!Vector<RTYPE>::is_na(x[i])){
          x_out[j] = x[i];
          first = 0;
        }
      }
      i++;
    }
    j++;
    first = 1;
  }
}

// [[Rcpp::export]]
DataFrame summarize_cpp(List v_list, CharacterVector groups, DataFrame df_in,
                        IntegerVector group_index, IntegerVector thrsh,
                        NumericVector W_to_MJ_interval) {
  int i;
  int j;
  int nrow_out = max(group_index);
  int ncol_out = 0;
  for(int el = 0; el < v_list.size(); el++){
    List element = v_list[el];
    ncol_out += as<CharacterVector>(element[".cols"]).size() *
      as<CharacterVector>(element[".fns"]).size();
  }
  ncol_out += groups.size();

  // Allocate list for output
  List df_out(ncol_out);

  // Construct names for output and allocate output columns
  CharacterVector col_names(ncol_out);
  i = 0;
  for(int g = 0; g < groups.size(); g++){
    col_names[i] = groups[g];
    df_out[i] = new_column(df_in[as<std::string>(groups[g])], nrow_out);
    if(is<DatetimeVector>(df_in[as<std::string>(groups[g])])){
      as<DatetimeVector>(df_out[i]).attr("tzone") =
        as<DatetimeVector>(df_in[as<std::string>(groups[g])]).attr("tzone");
    }
    i++;
  }
  for(int el = 0; el < v_list.size(); el++){
    List element = v_list[el];
    for(int v = 0; v < as<CharacterVector>(element[".cols"]).size(); v++){
      std::string vname = as<std::string>(as<CharacterVector>(element[".cols"])[v]);
      for(int s = 0; s < as<CharacterVector>(element[".fns"]).size(); s++){
        std::string sname = as<std::string>(as<CharacterVector>(element[".fns"])[s]);
        col_names[i] = vname + "_" + sname;
        df_out[i] = new_column(df_in[vname], nrow_out);
        if(as<NumericVector>(df_in[vname]).hasAttribute("units") &&
           sname != "pdir" && sname != "sdir"){
          if(vname == "SRAD" && sname == "srad_sum"){
            as<NumericVector>(df_out[i]).attr("units") =
              W_to_MJ_interval.attr("units");
          }else{
            as<NumericVector>(df_out[i]).attr("units") =
              as<NumericVector>(df_in[vname]).attr("units");
          }
          as<NumericVector>(df_out[i]).attr("class") =
            as<NumericVector>(df_in[vname]).attr("class");
        }
        i++;
      }
    }
  }
  df_out.names() = col_names;

  // Copy group variable values
  for(int g = 0; g < groups.size(); g++){
    if(is<CharacterVector>(df_in[as<std::string>(groups[g])])){
      CharacterVector x_char = df_in[as<std::string>(groups[g])];
      CharacterVector x_char_out = df_out[as<std::string>(groups[g])];
      copy_column(x_char, x_char_out, group_index);
    }else if(is<DatetimeVector>(df_in[as<std::string>(groups[g])])){
      DatetimeVector x_dt = df_in[as<std::string>(groups[g])];
      DatetimeVector x_dt_out = df_out[as<std::string>(groups[g])];
      copy_column(x_dt, x_dt_out, group_index);
    }else if(is<IntegerVector>(df_in[as<std::string>(groups[g])])){
      IntegerVector x_int = df_in[as<std::string>(groups[g])];
      IntegerVector x_int_out = df_out[as<std::string>(groups[g])];
      copy_column(x_int, x_int_out, group_index);
    }
  }

  // Perform summary
  for(int el = 0; el < v_list.size(); el++){
    List element = v_list[el];
    for(int v = 0; v < as<CharacterVector>(element[".cols"]).size(); v++){
      std::string vname = as<std::string>(as<CharacterVector>(element[".cols"])[v]);
      int min_count;
      if(vname == "SRAD"){
        min_count = thrsh["srad"];
      }else if(vname == "TS05" || vname == "TS10" || vname == "TS25" ||
               vname == "TS30" || vname == "TS60" || vname == "TB05" ||
               vname == "TB10"){
        min_count = thrsh["stemp"];
      }else if(vname == "TR05" || vname == "TR25" || vname == "TR60" ||
               vname == "TR75" || vname == "VW05" || vname == "VW25" ||
               vname == "VW45"){
        min_count = thrsh["smoist"];
      }else{
        min_count = thrsh["obs"];
      }
      IntegerVector wdir_count(16);
      for(int s = 0; s < as<CharacterVector>(element[".fns"]).size(); s++){
        std::string sname = as<std::string>(as<CharacterVector>(element[".fns"])[s]);
        std::string cname = vname + "_" + sname;
        NumericVector x = df_in[vname];
        NumericVector x_out = df_out[cname];
        i = 0; j = 0;
        while(j < nrow_out){
          int count = 0;
          reset_vec(wdir_count);
          x_out[j] = NumericVector::get_na();
          while(i < x.size() && group_index[i] == j + 1){
            if(!NumericVector::is_na(x[i])){
              count++;
              if(NumericVector::is_na(x_out[j])){
                if(sname == "mean" || sname == "min" || sname == "max" ||
                   sname == "sum" || sname == "srad_sum"){
                  x_out[j] = x[i];
                }else if(sname == "pdir" || sname == "sdir"){
                  wdir_count[round_wdir(x[i])]++;
                }
              }else{
                if(sname == "max"){
                  if(x_out[j] < x[i]) x_out[j] = x[i];
                }else if(sname == "min"){
                  if(x_out[j] > x[i]) x_out[j] = x[i];
                }else if(sname == "mean" || sname == "sum" ||
                         sname == "srad_sum"){
                  x_out[j] += x[i];
                }else if(sname == "pdir" || sname == "sdir"){
                  wdir_count[round_wdir(x[i])]++;
                }
              }
            }
            i++;
          }
          if(count < min_count){
            x_out[j] = NumericVector::get_na();
          }else{
            if(sname == "mean"){
              x_out[j] /= count;
            }else if(sname == "srad_sum"){
              x_out[j] = x_out[j]/count*W_to_MJ_interval[0];
            }else if(sname == "pdir"){
              x_out[j] = static_cast<double>(which_max(wdir_count)) + 1.;
            }else if(sname == "sdir"){
              int max_i = which_max(wdir_count);
              x_out[j] = static_cast<double>(max_i) + 1.;
              wdir_count[max_i] = 0;
              max_i = which_max(wdir_count);
              if(wdir_count[max_i] != 0){
                x_out[j] = static_cast<double>(max_i) + 1.;
              };
            }
          }
          j++;
        }
      }
    }
  }

  return DataFrame(df_out);
}

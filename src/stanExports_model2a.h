// Generated by rstantools.  Do not edit by hand.

/*
    sizedist is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    sizedist is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with sizedist.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.26.1-4-gd72b68b7-dirty
#include <stan/model/model_header.hpp>
namespace model_model2a_namespace {
inline void validate_positive_index(const char* var_name, const char* expr,
                                    int val) {
  if (val < 1) {
    std::stringstream msg;
    msg << "Found dimension size less than one in simplex declaration"
        << "; variable=" << var_name << "; dimension size expression=" << expr
        << "; expression value=" << val;
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
inline void validate_unit_vector_index(const char* var_name, const char* expr,
                                       int val) {
  if (val <= 1) {
    std::stringstream msg;
    if (val == 1) {
      msg << "Found dimension size one in unit vector declaration."
          << " One-dimensional unit vector is discrete"
          << " but the target distribution must be continuous."
          << " variable=" << var_name << "; dimension size expression=" << expr;
    } else {
      msg << "Found dimension size less than one in unit vector declaration"
          << "; variable=" << var_name << "; dimension size expression=" << expr
          << "; expression value=" << val;
    }
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using std::pow;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::model_base_crtp;
using stan::model::rvalue;
using stan::model::cons_list;
using stan::model::index_uni;
using stan::model::index_max;
using stan::model::index_min;
using stan::model::index_min_max;
using stan::model::index_multi;
using stan::model::index_omni;
using stan::model::nil_index_list;
using namespace stan::math;
using stan::math::pow; 
stan::math::profile_map profiles__;
static int current_statement__= 0;
static const std::vector<string> locations_array__ = {" (found before start of program)",
                                                      " (in 'model2a', line 15, column 2 to column 20)",
                                                      " (in 'model2a', line 16, column 2 to column 20)",
                                                      " (in 'model2a', line 19, column 18 to column 26)",
                                                      " (in 'model2a', line 19, column 2 to column 28)",
                                                      " (in 'model2a', line 21, column 2 to column 28)",
                                                      " (in 'model2a', line 22, column 2 to column 28)",
                                                      " (in 'model2a', line 25, column 4 to line 26, column 64)",
                                                      " (in 'model2a', line 24, column 23 to line 27, column 3)",
                                                      " (in 'model2a', line 24, column 2 to line 27, column 3)",
                                                      " (in 'model2a', line 28, column 2 to column 31)",
                                                      " (in 'model2a', line 4, column 2 to column 15)",
                                                      " (in 'model2a', line 5, column 17 to column 25)",
                                                      " (in 'model2a', line 5, column 2 to column 27)",
                                                      " (in 'model2a', line 6, column 17 to column 25)",
                                                      " (in 'model2a', line 6, column 2 to column 27)",
                                                      " (in 'model2a', line 7, column 13 to column 21)",
                                                      " (in 'model2a', line 7, column 2 to column 23)",
                                                      " (in 'model2a', line 9, column 2 to column 12)",
                                                      " (in 'model2a', line 10, column 2 to column 12)",
                                                      " (in 'model2a', line 11, column 2 to column 12)",
                                                      " (in 'model2a', line 12, column 2 to column 12)"};
#include <stan_meta_header.hpp>
class model_model2a final : public model_base_crtp<model_model2a> {
private:
  int N_counts;
  std::vector<double> bin_lower;
  std::vector<double> bin_upper;
  std::vector<int> counts;
  double c_mu;
  double c_sd;
  double b_mu;
  double b_sd;
 
public:
  ~model_model2a() { }
  
  inline std::string model_name() const final { return "model_model2a"; }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.26.1-4-gd72b68b7-dirty", "stancflags = "};
  }
  
  
  model_model2a(stan::io::var_context& context__,
                unsigned int random_seed__ = 0,
                std::ostream* pstream__ = nullptr) : model_base_crtp(0) {
    using local_scalar_t__ = double ;
    boost::ecuyer1988 base_rng__ = 
        stan::services::util::create_rng(random_seed__, 0);
    (void) base_rng__;  // suppress unused var warning
    static const char* function__ = "model_model2a_namespace::model_model2a";
    (void) function__;  // suppress unused var warning
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      current_statement__ = 11;
      context__.validate_dims("data initialization","N_counts","int",
          context__.to_vec());
      N_counts = std::numeric_limits<int>::min();
      
      current_statement__ = 11;
      N_counts = context__.vals_i("N_counts")[(1 - 1)];
      current_statement__ = 12;
      validate_non_negative_index("bin_lower", "N_counts", N_counts);
      current_statement__ = 13;
      context__.validate_dims("data initialization","bin_lower","double",
          context__.to_vec(N_counts));
      bin_lower = std::vector<double>(N_counts, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 13;
      assign(bin_lower, nil_index_list(), context__.vals_r("bin_lower"),
        "assigning variable bin_lower");
      current_statement__ = 14;
      validate_non_negative_index("bin_upper", "N_counts", N_counts);
      current_statement__ = 15;
      context__.validate_dims("data initialization","bin_upper","double",
          context__.to_vec(N_counts));
      bin_upper = std::vector<double>(N_counts, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 15;
      assign(bin_upper, nil_index_list(), context__.vals_r("bin_upper"),
        "assigning variable bin_upper");
      current_statement__ = 16;
      validate_non_negative_index("counts", "N_counts", N_counts);
      current_statement__ = 17;
      context__.validate_dims("data initialization","counts","int",
          context__.to_vec(N_counts));
      counts = std::vector<int>(N_counts, std::numeric_limits<int>::min());
      
      current_statement__ = 17;
      assign(counts, nil_index_list(), context__.vals_i("counts"),
        "assigning variable counts");
      current_statement__ = 18;
      context__.validate_dims("data initialization","c_mu","double",
          context__.to_vec());
      c_mu = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 18;
      c_mu = context__.vals_r("c_mu")[(1 - 1)];
      current_statement__ = 19;
      context__.validate_dims("data initialization","c_sd","double",
          context__.to_vec());
      c_sd = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 19;
      c_sd = context__.vals_r("c_sd")[(1 - 1)];
      current_statement__ = 20;
      context__.validate_dims("data initialization","b_mu","double",
          context__.to_vec());
      b_mu = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 20;
      b_mu = context__.vals_r("b_mu")[(1 - 1)];
      current_statement__ = 21;
      context__.validate_dims("data initialization","b_sd","double",
          context__.to_vec());
      b_sd = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 21;
      b_sd = context__.vals_r("b_sd")[(1 - 1)];
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    num_params_r__ = 0U;
    
    try {
      num_params_r__ += 1;
      num_params_r__ += 1;
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI, stan::require_vector_like_t<VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR> log_prob_impl(VecR& params_r__,
                                                 VecI& params_i__,
                                                 std::ostream* pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    static const char* function__ = "model_model2a_namespace::log_prob";
(void) function__;  // suppress unused var warning
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      local_scalar_t__ b;
      b = DUMMY_VAR__;
      
      current_statement__ = 1;
      b = in__.scalar();
      current_statement__ = 1;
      if (jacobian__) {
        current_statement__ = 1;
        b = stan::math::lb_constrain(b, 0, lp__);
      } else {
        current_statement__ = 1;
        b = stan::math::lb_constrain(b, 0);
      }
      local_scalar_t__ c;
      c = DUMMY_VAR__;
      
      current_statement__ = 2;
      c = in__.scalar();
      current_statement__ = 2;
      if (jacobian__) {
        current_statement__ = 2;
        c = stan::math::lb_constrain(c, 0, lp__);
      } else {
        current_statement__ = 2;
        c = stan::math::lb_constrain(c, 0);
      }
      {
        current_statement__ = 3;
        validate_non_negative_index("counts_est", "N_counts", N_counts);
        std::vector<local_scalar_t__> counts_est;
        counts_est = std::vector<local_scalar_t__>(N_counts, DUMMY_VAR__);
        
        current_statement__ = 5;
        lp_accum__.add(lognormal_lpdf<propto__>(c, c_mu, c_sd));
        current_statement__ = 6;
        lp_accum__.add(lognormal_lpdf<propto__>(b, b_mu, b_sd));
        current_statement__ = 9;
        for (int i = 1; i <= N_counts; ++i) {
          current_statement__ = 7;
          assign(counts_est, cons_list(index_uni(i), nil_index_list()),
            ((c / (1 - b)) *
              (pow(bin_upper[(i - 1)], (1 - b)) -
                pow(bin_lower[(i - 1)], (1 - b)))),
            "assigning variable counts_est");}
        current_statement__ = 10;
        lp_accum__.add(poisson_lpmf<propto__>(counts, counts_est));
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
    } // log_prob_impl() 
    
  template <typename RNG, typename VecR, typename VecI, typename VecVar, stan::require_vector_like_vt<std::is_floating_point, VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr, stan::require_std_vector_vt<std::is_floating_point, VecVar>* = nullptr>
  inline void write_array_impl(RNG& base_rng__, VecR& params_r__,
                               VecI& params_i__, VecVar& vars__,
                               const bool emit_transformed_parameters__ = true,
                               const bool emit_generated_quantities__ = true,
                               std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.resize(0);
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    static const char* function__ = "model_model2a_namespace::write_array";
(void) function__;  // suppress unused var warning
    (void) function__;  // suppress unused var warning
    double lp__ = 0.0;
    (void) lp__;  // dummy to suppress unused var warning
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      double b;
      b = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      b = in__.scalar();
      current_statement__ = 1;
      b = stan::math::lb_constrain(b, 0);
      double c;
      c = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      c = in__.scalar();
      current_statement__ = 2;
      c = stan::math::lb_constrain(c, 0);
      vars__.emplace_back(b);
      vars__.emplace_back(c);
      if (logical_negation((primitive_value(emit_transformed_parameters__) ||
            primitive_value(emit_generated_quantities__)))) {
        return ;
      } 
      if (logical_negation(emit_generated_quantities__)) {
        return ;
      } 
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // write_array_impl() 
    
  template <typename VecVar, typename VecI, stan::require_std_vector_t<VecVar>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void transform_inits_impl(const stan::io::var_context& context__,
                                   VecI& params_i__, VecVar& vars__,
                                   std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.clear();
    vars__.reserve(num_params_r__);
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      double b;
      b = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      b = context__.vals_r("b")[(1 - 1)];
      double b_free__;
      b_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      b_free__ = stan::math::lb_free(b, 0);
      double c;
      c = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      c = context__.vals_r("c")[(1 - 1)];
      double c_free__;
      c_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      c_free__ = stan::math::lb_free(c, 0);
      vars__.emplace_back(b_free__);
      vars__.emplace_back(c_free__);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // transform_inits_impl() 
    
  inline void get_param_names(std::vector<std::string>& names__) const {
    
    names__.clear();
    names__.emplace_back("b");
    names__.emplace_back("c");
    } // get_param_names() 
    
  inline void get_dims(std::vector<std::vector<size_t>>& dimss__) const {
    dimss__.clear();
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    } // get_dims() 
    
  inline void constrained_param_names(
                                      std::vector<std::string>& param_names__,
                                      bool emit_transformed_parameters__ = true,
                                      bool emit_generated_quantities__ = true) const
    final {
    
    param_names__.emplace_back(std::string() + "b");
    param_names__.emplace_back(std::string() + "c");
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      
    }
    
    } // constrained_param_names() 
    
  inline void unconstrained_param_names(
                                        std::vector<std::string>& param_names__,
                                        bool emit_transformed_parameters__ = true,
                                        bool emit_generated_quantities__ = true) const
    final {
    
    param_names__.emplace_back(std::string() + "b");
    param_names__.emplace_back(std::string() + "c");
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      
    }
    
    } // unconstrained_param_names() 
    
  inline std::string get_constrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"b\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"c\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"}]";
    return s__.str();
    } // get_constrained_sizedtypes() 
    
  inline std::string get_unconstrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"b\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"c\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"}]";
    return s__.str();
    } // get_unconstrained_sizedtypes() 
    
  
    // Begin method overload boilerplate
    template <typename RNG>
    inline void write_array(RNG& base_rng,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                            const bool emit_transformed_parameters = true,
                            const bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      std::vector<double> vars_vec(vars.size());
      std::vector<int> params_i;
      write_array_impl(base_rng, params_r, params_i, vars_vec,
          emit_transformed_parameters, emit_generated_quantities, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i) {
        vars.coeffRef(i) = vars_vec[i];
      }
    }
    template <typename RNG>
    inline void write_array(RNG& base_rng, std::vector<double>& params_r,
                            std::vector<int>& params_i,
                            std::vector<double>& vars,
                            bool emit_transformed_parameters = true,
                            bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      write_array_impl(base_rng, params_r, params_i, vars, emit_transformed_parameters, emit_generated_quantities, pstream);
    }
    template <bool propto__, bool jacobian__, typename T_>
    inline T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
                       std::ostream* pstream = nullptr) const {
      Eigen::Matrix<int, -1, 1> params_i;
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
    template <bool propto__, bool jacobian__, typename T__>
    inline T__ log_prob(std::vector<T__>& params_r,
                        std::vector<int>& params_i,
                        std::ostream* pstream = nullptr) const {
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
  
    inline void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream = nullptr) const final {
      std::vector<double> params_r_vec(params_r.size());
      std::vector<int> params_i;
      transform_inits_impl(context, params_i, params_r_vec, pstream);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i) {
        params_r.coeffRef(i) = params_r_vec[i];
      }
    }
    inline void transform_inits(const stan::io::var_context& context,
                                std::vector<int>& params_i,
                                std::vector<double>& vars,
                                std::ostream* pstream = nullptr) const final {
      transform_inits_impl(context, params_i, vars, pstream);
    }        
};
}
using stan_model = model_model2a_namespace::model_model2a;
#ifndef USING_R
// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_model2a_namespace::profiles__;
}
#endif
#endif
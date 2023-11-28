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
namespace model_model2b_namespace {
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
                                                      " (in 'model2b', line 27, column 2 to column 34)",
                                                      " (in 'model2b', line 28, column 2 to column 34)",
                                                      " (in 'model2b', line 29, column 2 to column 34)",
                                                      " (in 'model2b', line 30, column 2 to column 21)",
                                                      " (in 'model2b', line 31, column 2 to column 27)",
                                                      " (in 'model2b', line 34, column 18 to column 26)",
                                                      " (in 'model2b', line 34, column 2 to column 28)",
                                                      " (in 'model2b', line 35, column 16 to column 24)",
                                                      " (in 'model2b', line 35, column 2 to column 26)",
                                                      " (in 'model2b', line 37, column 2 to column 28)",
                                                      " (in 'model2b', line 38, column 2 to column 28)",
                                                      " (in 'model2b', line 39, column 2 to column 28)",
                                                      " (in 'model2b', line 40, column 2 to column 28)",
                                                      " (in 'model2b', line 41, column 2 to column 40)",
                                                      " (in 'model2b', line 44, column 4 to line 45, column 82)",
                                                      " (in 'model2b', line 43, column 23 to line 46, column 3)",
                                                      " (in 'model2b', line 43, column 2 to line 46, column 3)",
                                                      " (in 'model2b', line 47, column 2 to column 31)",
                                                      " (in 'model2b', line 50, column 4 to column 37)",
                                                      " (in 'model2b', line 49, column 23 to line 51, column 3)",
                                                      " (in 'model2b', line 49, column 2 to line 51, column 3)",
                                                      " (in 'model2b', line 53, column 2 to column 42)",
                                                      " (in 'model2b', line 4, column 2 to column 15)",
                                                      " (in 'model2b', line 5, column 17 to column 25)",
                                                      " (in 'model2b', line 5, column 2 to column 27)",
                                                      " (in 'model2b', line 6, column 17 to column 25)",
                                                      " (in 'model2b', line 6, column 2 to column 27)",
                                                      " (in 'model2b', line 7, column 13 to column 21)",
                                                      " (in 'model2b', line 7, column 2 to column 23)",
                                                      " (in 'model2b', line 9, column 2 to column 24)",
                                                      " (in 'model2b', line 10, column 9 to column 17)",
                                                      " (in 'model2b', line 10, column 2 to column 23)",
                                                      " (in 'model2b', line 11, column 9 to column 17)",
                                                      " (in 'model2b', line 11, column 2 to column 28)",
                                                      " (in 'model2b', line 13, column 2 to column 12)",
                                                      " (in 'model2b', line 14, column 2 to column 12)",
                                                      " (in 'model2b', line 15, column 2 to column 12)",
                                                      " (in 'model2b', line 16, column 2 to column 12)",
                                                      " (in 'model2b', line 17, column 2 to column 12)",
                                                      " (in 'model2b', line 18, column 2 to column 12)",
                                                      " (in 'model2b', line 19, column 2 to column 12)",
                                                      " (in 'model2b', line 20, column 2 to column 12)",
                                                      " (in 'model2b', line 21, column 2 to column 12)",
                                                      " (in 'model2b', line 22, column 2 to column 13)",
                                                      " (in 'model2b', line 23, column 2 to column 13)",
                                                      " (in 'model2b', line 24, column 2 to column 21)"};
#include <stan_meta_header.hpp>
class model_model2b final : public model_base_crtp<model_model2b> {
private:
  int N_counts;
  std::vector<double> bin_lower;
  std::vector<double> bin_upper;
  std::vector<int> counts;
  int N_growth;
  Eigen::Matrix<double, -1, 1> age;
  Eigen::Matrix<double, -1, 1> size_ind;
  double Z_mu;
  double Z_sd;
  double Z_up;
  double g_mu;
  double g_sd;
  double g_up;
  double R_mu;
  double R_sd;
  double R_up;
  double s0_mu;
  double s0_sd;
  double sigma_size_sd;
 
public:
  ~model_model2b() { }
  
  inline std::string model_name() const final { return "model_model2b"; }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.26.1-4-gd72b68b7-dirty", "stancflags = "};
  }
  
  
  model_model2b(stan::io::var_context& context__,
                unsigned int random_seed__ = 0,
                std::ostream* pstream__ = nullptr) : model_base_crtp(0) {
    using local_scalar_t__ = double ;
    boost::ecuyer1988 base_rng__ = 
        stan::services::util::create_rng(random_seed__, 0);
    (void) base_rng__;  // suppress unused var warning
    static const char* function__ = "model_model2b_namespace::model_model2b";
    (void) function__;  // suppress unused var warning
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      current_statement__ = 23;
      context__.validate_dims("data initialization","N_counts","int",
          context__.to_vec());
      N_counts = std::numeric_limits<int>::min();
      
      current_statement__ = 23;
      N_counts = context__.vals_i("N_counts")[(1 - 1)];
      current_statement__ = 24;
      validate_non_negative_index("bin_lower", "N_counts", N_counts);
      current_statement__ = 25;
      context__.validate_dims("data initialization","bin_lower","double",
          context__.to_vec(N_counts));
      bin_lower = std::vector<double>(N_counts, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 25;
      assign(bin_lower, nil_index_list(), context__.vals_r("bin_lower"),
        "assigning variable bin_lower");
      current_statement__ = 26;
      validate_non_negative_index("bin_upper", "N_counts", N_counts);
      current_statement__ = 27;
      context__.validate_dims("data initialization","bin_upper","double",
          context__.to_vec(N_counts));
      bin_upper = std::vector<double>(N_counts, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 27;
      assign(bin_upper, nil_index_list(), context__.vals_r("bin_upper"),
        "assigning variable bin_upper");
      current_statement__ = 28;
      validate_non_negative_index("counts", "N_counts", N_counts);
      current_statement__ = 29;
      context__.validate_dims("data initialization","counts","int",
          context__.to_vec(N_counts));
      counts = std::vector<int>(N_counts, std::numeric_limits<int>::min());
      
      current_statement__ = 29;
      assign(counts, nil_index_list(), context__.vals_i("counts"),
        "assigning variable counts");
      current_statement__ = 30;
      context__.validate_dims("data initialization","N_growth","int",
          context__.to_vec());
      N_growth = std::numeric_limits<int>::min();
      
      current_statement__ = 30;
      N_growth = context__.vals_i("N_growth")[(1 - 1)];
      current_statement__ = 30;
      current_statement__ = 30;
      check_greater_or_equal(function__, "N_growth", N_growth, 0);
      current_statement__ = 31;
      validate_non_negative_index("age", "N_growth", N_growth);
      current_statement__ = 32;
      context__.validate_dims("data initialization","age","double",
          context__.to_vec(N_growth));
      age = Eigen::Matrix<double, -1, 1>(N_growth);
      stan::math::fill(age, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> age_flat__;
        current_statement__ = 32;
        assign(age_flat__, nil_index_list(), context__.vals_r("age"),
          "assigning variable age_flat__");
        current_statement__ = 32;
        pos__ = 1;
        current_statement__ = 32;
        for (int sym1__ = 1; sym1__ <= N_growth; ++sym1__) {
          current_statement__ = 32;
          assign(age, cons_list(index_uni(sym1__), nil_index_list()),
            age_flat__[(pos__ - 1)], "assigning variable age");
          current_statement__ = 32;
          pos__ = (pos__ + 1);}
      }
      current_statement__ = 33;
      validate_non_negative_index("size_ind", "N_growth", N_growth);
      current_statement__ = 34;
      context__.validate_dims("data initialization","size_ind","double",
          context__.to_vec(N_growth));
      size_ind = Eigen::Matrix<double, -1, 1>(N_growth);
      stan::math::fill(size_ind, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> size_ind_flat__;
        current_statement__ = 34;
        assign(size_ind_flat__, nil_index_list(),
          context__.vals_r("size_ind"), "assigning variable size_ind_flat__");
        current_statement__ = 34;
        pos__ = 1;
        current_statement__ = 34;
        for (int sym1__ = 1; sym1__ <= N_growth; ++sym1__) {
          current_statement__ = 34;
          assign(size_ind, cons_list(index_uni(sym1__), nil_index_list()),
            size_ind_flat__[(pos__ - 1)], "assigning variable size_ind");
          current_statement__ = 34;
          pos__ = (pos__ + 1);}
      }
      current_statement__ = 35;
      context__.validate_dims("data initialization","Z_mu","double",
          context__.to_vec());
      Z_mu = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 35;
      Z_mu = context__.vals_r("Z_mu")[(1 - 1)];
      current_statement__ = 36;
      context__.validate_dims("data initialization","Z_sd","double",
          context__.to_vec());
      Z_sd = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 36;
      Z_sd = context__.vals_r("Z_sd")[(1 - 1)];
      current_statement__ = 37;
      context__.validate_dims("data initialization","Z_up","double",
          context__.to_vec());
      Z_up = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 37;
      Z_up = context__.vals_r("Z_up")[(1 - 1)];
      current_statement__ = 38;
      context__.validate_dims("data initialization","g_mu","double",
          context__.to_vec());
      g_mu = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 38;
      g_mu = context__.vals_r("g_mu")[(1 - 1)];
      current_statement__ = 39;
      context__.validate_dims("data initialization","g_sd","double",
          context__.to_vec());
      g_sd = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 39;
      g_sd = context__.vals_r("g_sd")[(1 - 1)];
      current_statement__ = 40;
      context__.validate_dims("data initialization","g_up","double",
          context__.to_vec());
      g_up = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 40;
      g_up = context__.vals_r("g_up")[(1 - 1)];
      current_statement__ = 41;
      context__.validate_dims("data initialization","R_mu","double",
          context__.to_vec());
      R_mu = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 41;
      R_mu = context__.vals_r("R_mu")[(1 - 1)];
      current_statement__ = 42;
      context__.validate_dims("data initialization","R_sd","double",
          context__.to_vec());
      R_sd = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 42;
      R_sd = context__.vals_r("R_sd")[(1 - 1)];
      current_statement__ = 43;
      context__.validate_dims("data initialization","R_up","double",
          context__.to_vec());
      R_up = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 43;
      R_up = context__.vals_r("R_up")[(1 - 1)];
      current_statement__ = 44;
      context__.validate_dims("data initialization","s0_mu","double",
          context__.to_vec());
      s0_mu = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 44;
      s0_mu = context__.vals_r("s0_mu")[(1 - 1)];
      current_statement__ = 45;
      context__.validate_dims("data initialization","s0_sd","double",
          context__.to_vec());
      s0_sd = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 45;
      s0_sd = context__.vals_r("s0_sd")[(1 - 1)];
      current_statement__ = 46;
      context__.validate_dims("data initialization","sigma_size_sd","double",
          context__.to_vec());
      sigma_size_sd = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 46;
      sigma_size_sd = context__.vals_r("sigma_size_sd")[(1 - 1)];
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    num_params_r__ = 0U;
    
    try {
      num_params_r__ += 1;
      num_params_r__ += 1;
      num_params_r__ += 1;
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
    static const char* function__ = "model_model2b_namespace::log_prob";
(void) function__;  // suppress unused var warning
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      local_scalar_t__ Z;
      Z = DUMMY_VAR__;
      
      current_statement__ = 1;
      Z = in__.scalar();
      current_statement__ = 1;
      if (jacobian__) {
        current_statement__ = 1;
        Z = stan::math::lub_constrain(Z, 0, Z_up, lp__);
      } else {
        current_statement__ = 1;
        Z = stan::math::lub_constrain(Z, 0, Z_up);
      }
      local_scalar_t__ R;
      R = DUMMY_VAR__;
      
      current_statement__ = 2;
      R = in__.scalar();
      current_statement__ = 2;
      if (jacobian__) {
        current_statement__ = 2;
        R = stan::math::lub_constrain(R, 0, R_up, lp__);
      } else {
        current_statement__ = 2;
        R = stan::math::lub_constrain(R, 0, R_up);
      }
      local_scalar_t__ g;
      g = DUMMY_VAR__;
      
      current_statement__ = 3;
      g = in__.scalar();
      current_statement__ = 3;
      if (jacobian__) {
        current_statement__ = 3;
        g = stan::math::lub_constrain(g, 0, g_up, lp__);
      } else {
        current_statement__ = 3;
        g = stan::math::lub_constrain(g, 0, g_up);
      }
      local_scalar_t__ s0;
      s0 = DUMMY_VAR__;
      
      current_statement__ = 4;
      s0 = in__.scalar();
      current_statement__ = 4;
      if (jacobian__) {
        current_statement__ = 4;
        s0 = stan::math::lb_constrain(s0, 0, lp__);
      } else {
        current_statement__ = 4;
        s0 = stan::math::lb_constrain(s0, 0);
      }
      local_scalar_t__ sigma_size;
      sigma_size = DUMMY_VAR__;
      
      current_statement__ = 5;
      sigma_size = in__.scalar();
      current_statement__ = 5;
      if (jacobian__) {
        current_statement__ = 5;
        sigma_size = stan::math::lb_constrain(sigma_size, 0, lp__);
      } else {
        current_statement__ = 5;
        sigma_size = stan::math::lb_constrain(sigma_size, 0);
      }
      {
        current_statement__ = 6;
        validate_non_negative_index("counts_est", "N_counts", N_counts);
        std::vector<local_scalar_t__> counts_est;
        counts_est = std::vector<local_scalar_t__>(N_counts, DUMMY_VAR__);
        
        current_statement__ = 8;
        validate_non_negative_index("size_est", "N_growth", N_growth);
        std::vector<local_scalar_t__> size_est;
        size_est = std::vector<local_scalar_t__>(N_growth, DUMMY_VAR__);
        
        current_statement__ = 10;
        lp_accum__.add(lognormal_lpdf<propto__>(Z, Z_mu, Z_sd));
        current_statement__ = 11;
        lp_accum__.add(lognormal_lpdf<propto__>(g, g_mu, g_sd));
        current_statement__ = 12;
        lp_accum__.add(lognormal_lpdf<propto__>(R, R_mu, R_sd));
        current_statement__ = 13;
        lp_accum__.add(cauchy_lpdf<propto__>(s0, s0_mu, s0_sd));
        current_statement__ = 14;
        lp_accum__.add(cauchy_lpdf<propto__>(sigma_size, 0, sigma_size_sd));
        current_statement__ = 17;
        for (int i = 1; i <= N_counts; ++i) {
          current_statement__ = 15;
          assign(counts_est, cons_list(index_uni(i), nil_index_list()),
            (((-R / Z) * pow(s0, (Z / g))) *
              (pow(bin_upper[(i - 1)], -(Z / g)) -
                pow(bin_lower[(i - 1)], (-Z / g)))),
            "assigning variable counts_est");}
        current_statement__ = 18;
        lp_accum__.add(poisson_lpmf<propto__>(counts, counts_est));
        current_statement__ = 21;
        for (int i = 1; i <= N_growth; ++i) {
          current_statement__ = 19;
          assign(size_est, cons_list(index_uni(i), nil_index_list()),
            (s0 * stan::math::exp((age[(i - 1)] * g))),
            "assigning variable size_est");}
        current_statement__ = 22;
        lp_accum__.add(normal_lpdf<propto__>(size_ind, size_est, sigma_size));
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
    static const char* function__ = "model_model2b_namespace::write_array";
(void) function__;  // suppress unused var warning
    (void) function__;  // suppress unused var warning
    double lp__ = 0.0;
    (void) lp__;  // dummy to suppress unused var warning
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      double Z;
      Z = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      Z = in__.scalar();
      current_statement__ = 1;
      Z = stan::math::lub_constrain(Z, 0, Z_up);
      double R;
      R = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      R = in__.scalar();
      current_statement__ = 2;
      R = stan::math::lub_constrain(R, 0, R_up);
      double g;
      g = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 3;
      g = in__.scalar();
      current_statement__ = 3;
      g = stan::math::lub_constrain(g, 0, g_up);
      double s0;
      s0 = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 4;
      s0 = in__.scalar();
      current_statement__ = 4;
      s0 = stan::math::lb_constrain(s0, 0);
      double sigma_size;
      sigma_size = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 5;
      sigma_size = in__.scalar();
      current_statement__ = 5;
      sigma_size = stan::math::lb_constrain(sigma_size, 0);
      vars__.emplace_back(Z);
      vars__.emplace_back(R);
      vars__.emplace_back(g);
      vars__.emplace_back(s0);
      vars__.emplace_back(sigma_size);
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
      double Z;
      Z = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      Z = context__.vals_r("Z")[(1 - 1)];
      double Z_free__;
      Z_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 1;
      Z_free__ = stan::math::lub_free(Z, 0, Z_up);
      double R;
      R = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      R = context__.vals_r("R")[(1 - 1)];
      double R_free__;
      R_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 2;
      R_free__ = stan::math::lub_free(R, 0, R_up);
      double g;
      g = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 3;
      g = context__.vals_r("g")[(1 - 1)];
      double g_free__;
      g_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 3;
      g_free__ = stan::math::lub_free(g, 0, g_up);
      double s0;
      s0 = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 4;
      s0 = context__.vals_r("s0")[(1 - 1)];
      double s0_free__;
      s0_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 4;
      s0_free__ = stan::math::lb_free(s0, 0);
      double sigma_size;
      sigma_size = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 5;
      sigma_size = context__.vals_r("sigma_size")[(1 - 1)];
      double sigma_size_free__;
      sigma_size_free__ = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 5;
      sigma_size_free__ = stan::math::lb_free(sigma_size, 0);
      vars__.emplace_back(Z_free__);
      vars__.emplace_back(R_free__);
      vars__.emplace_back(g_free__);
      vars__.emplace_back(s0_free__);
      vars__.emplace_back(sigma_size_free__);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // transform_inits_impl() 
    
  inline void get_param_names(std::vector<std::string>& names__) const {
    
    names__.clear();
    names__.emplace_back("Z");
    names__.emplace_back("R");
    names__.emplace_back("g");
    names__.emplace_back("s0");
    names__.emplace_back("sigma_size");
    } // get_param_names() 
    
  inline void get_dims(std::vector<std::vector<size_t>>& dimss__) const {
    dimss__.clear();
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    dimss__.emplace_back(std::vector<size_t>{});
    
    } // get_dims() 
    
  inline void constrained_param_names(
                                      std::vector<std::string>& param_names__,
                                      bool emit_transformed_parameters__ = true,
                                      bool emit_generated_quantities__ = true) const
    final {
    
    param_names__.emplace_back(std::string() + "Z");
    param_names__.emplace_back(std::string() + "R");
    param_names__.emplace_back(std::string() + "g");
    param_names__.emplace_back(std::string() + "s0");
    param_names__.emplace_back(std::string() + "sigma_size");
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
    
    param_names__.emplace_back(std::string() + "Z");
    param_names__.emplace_back(std::string() + "R");
    param_names__.emplace_back(std::string() + "g");
    param_names__.emplace_back(std::string() + "s0");
    param_names__.emplace_back(std::string() + "sigma_size");
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      
    }
    
    } // unconstrained_param_names() 
    
  inline std::string get_constrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"Z\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"R\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"g\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"s0\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma_size\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"}]";
    return s__.str();
    } // get_constrained_sizedtypes() 
    
  inline std::string get_unconstrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"Z\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"R\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"g\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"s0\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma_size\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"}]";
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
using stan_model = model_model2b_namespace::model_model2b;
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
  return model_model2b_namespace::profiles__;
}
#endif
#endif

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
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_mortality_size_known_g_wprior_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_mortality_size_known_g_wprior");
    reader.add_event(37, 35, "end", "model_mortality_size_known_g_wprior");
    return reader;
}
#include <stan_meta_header.hpp>
class model_mortality_size_known_g_wprior
  : public stan::model::model_base_crtp<model_mortality_size_known_g_wprior> {
private:
        int N_counts;
        double s0_av;
        double g_av;
        std::vector<double> bin_lower;
        std::vector<double> bin_upper;
        std::vector<int> counts;
public:
    model_mortality_size_known_g_wprior(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_mortality_size_known_g_wprior(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_mortality_size_known_g_wprior_namespace::model_mortality_size_known_g_wprior";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 4;
            context__.validate_dims("data initialization", "N_counts", "int", context__.to_vec());
            N_counts = int(0);
            vals_i__ = context__.vals_i("N_counts");
            pos__ = 0;
            N_counts = vals_i__[pos__++];
            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "s0_av", "double", context__.to_vec());
            s0_av = double(0);
            vals_r__ = context__.vals_r("s0_av");
            pos__ = 0;
            s0_av = vals_r__[pos__++];
            current_statement_begin__ = 6;
            context__.validate_dims("data initialization", "g_av", "double", context__.to_vec());
            g_av = double(0);
            vals_r__ = context__.vals_r("g_av");
            pos__ = 0;
            g_av = vals_r__[pos__++];
            current_statement_begin__ = 7;
            validate_non_negative_index("bin_lower", "N_counts", N_counts);
            context__.validate_dims("data initialization", "bin_lower", "double", context__.to_vec(N_counts));
            bin_lower = std::vector<double>(N_counts, double(0));
            vals_r__ = context__.vals_r("bin_lower");
            pos__ = 0;
            size_t bin_lower_k_0_max__ = N_counts;
            for (size_t k_0__ = 0; k_0__ < bin_lower_k_0_max__; ++k_0__) {
                bin_lower[k_0__] = vals_r__[pos__++];
            }
            current_statement_begin__ = 8;
            validate_non_negative_index("bin_upper", "N_counts", N_counts);
            context__.validate_dims("data initialization", "bin_upper", "double", context__.to_vec(N_counts));
            bin_upper = std::vector<double>(N_counts, double(0));
            vals_r__ = context__.vals_r("bin_upper");
            pos__ = 0;
            size_t bin_upper_k_0_max__ = N_counts;
            for (size_t k_0__ = 0; k_0__ < bin_upper_k_0_max__; ++k_0__) {
                bin_upper[k_0__] = vals_r__[pos__++];
            }
            current_statement_begin__ = 9;
            validate_non_negative_index("counts", "N_counts", N_counts);
            context__.validate_dims("data initialization", "counts", "int", context__.to_vec(N_counts));
            counts = std::vector<int>(N_counts, int(0));
            vals_i__ = context__.vals_i("counts");
            pos__ = 0;
            size_t counts_k_0_max__ = N_counts;
            for (size_t k_0__ = 0; k_0__ < counts_k_0_max__; ++k_0__) {
                counts[k_0__] = vals_i__[pos__++];
            }
            // initialize transformed data variables
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 13;
            num_params_r__ += 1;
            current_statement_begin__ = 14;
            num_params_r__ += 1;
            current_statement_begin__ = 15;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_mortality_size_known_g_wprior() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 13;
        if (!(context__.contains_r("Z")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable Z missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("Z");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "Z", "double", context__.to_vec());
        double Z(0);
        Z = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, Z);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable Z: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 14;
        if (!(context__.contains_r("R")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable R missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("R");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "R", "double", context__.to_vec());
        double R(0);
        R = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, R);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable R: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 15;
        if (!(context__.contains_r("g")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable g missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("g");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "g", "double", context__.to_vec());
        double g(0);
        g = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, g);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable g: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 13;
            local_scalar_t__ Z;
            (void) Z;  // dummy to suppress unused var warning
            if (jacobian__)
                Z = in__.scalar_lb_constrain(0, lp__);
            else
                Z = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 14;
            local_scalar_t__ R;
            (void) R;  // dummy to suppress unused var warning
            if (jacobian__)
                R = in__.scalar_lb_constrain(0, lp__);
            else
                R = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 15;
            local_scalar_t__ g;
            (void) g;  // dummy to suppress unused var warning
            if (jacobian__)
                g = in__.scalar_lb_constrain(0, lp__);
            else
                g = in__.scalar_lb_constrain(0);
            // model body
            {
            current_statement_begin__ = 20;
            validate_non_negative_index("counts_est", "N_counts", N_counts);
            std::vector<local_scalar_t__  > counts_est(N_counts, local_scalar_t__(DUMMY_VAR__));
            stan::math::initialize(counts_est, DUMMY_VAR__);
            stan::math::fill(counts_est, DUMMY_VAR__);
            current_statement_begin__ = 23;
            lp_accum__.add(cauchy_log<propto__>(Z, 0, 10));
            current_statement_begin__ = 24;
            lp_accum__.add(normal_log<propto__>(g, g_av, 0.0001));
            current_statement_begin__ = 25;
            lp_accum__.add(cauchy_log<propto__>(R, 0, 100));
            current_statement_begin__ = 28;
            for (int i = 1; i <= N_counts; ++i) {
                current_statement_begin__ = 29;
                stan::model::assign(counts_est, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            ((-(R) / Z) * (stan::math::exp(((-(Z) / g) * (get_base1(bin_upper, i, "bin_upper", 1) - s0_av))) - stan::math::exp(((-(Z) / g) * (get_base1(bin_lower, i, "bin_lower", 1) - s0_av))))), 
                            "assigning variable counts_est");
            }
            current_statement_begin__ = 34;
            lp_accum__.add(poisson_log<propto__>(counts, counts_est));
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("Z");
        names__.push_back("R");
        names__.push_back("g");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_mortality_size_known_g_wprior_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        double Z = in__.scalar_lb_constrain(0);
        vars__.push_back(Z);
        double R = in__.scalar_lb_constrain(0);
        vars__.push_back(R);
        double g = in__.scalar_lb_constrain(0);
        vars__.push_back(g);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_mortality_size_known_g_wprior";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "Z";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "R";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "g";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "Z";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "R";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "g";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
    }
}; // model
}  // namespace
typedef model_mortality_size_known_g_wprior_namespace::model_mortality_size_known_g_wprior stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif

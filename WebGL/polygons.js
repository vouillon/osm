// This program was compiled from OCaml by js_of_ocaml 1.99dev
(function(joo_global_object_c_)
   {"use strict";
    var
     _a6_=125,
     _as_=123,
     _cs_=254,
     _p_=255,
     _ci_="x",
     _ax_=".",
     _bd_=108,
     _cf_=-346804178,
     _av_="+",
     _aw_=65535,
     _q_=16777215,
     _ch_="g",
     _a5_="f",
     _co_=250,
     _I_=105,
     _bc_=0.5,
     _cn_=0.9,
     _Z_=110,
     ___=115,
     _ar_="int_of_string",
     _ba_=102,
     _bb_=512,
     _au_=111,
     _a8_=120,
     _r_=" ",
     _Y_="e",
     _a7_=117,
     _a9_=118,
     _H_="-",
     _cl_="nan",
     _i_="",
     _cr_=128,
     _a4_=116,
     _ay_=100,
     _m_="0",
     _cj_=271466201,
     _cq_=192195741,
     _at_=114,
     _a$_=103,
     _cd_=892709484,
     _cp_=101,
     _cg_=0.1,
     _ce_=736157203,
     _ck_="number",
     _cm_=724911760,
     _a__=1e3;
    function caml_sys_const_word_size_e0_(){return 32}
    function caml_register_named_value_eZ_(nm_a_,v_b_)
     {caml_named_values_eR_[nm_a_]=v_b_;return 0}
    var caml_named_values_eR_={};
    function caml_register_global_eY_(n_a_,v_b_)
     {caml_global_data_aC_[n_a_+1]=v_b_}
    function caml_notequal_eT_(x_a_,y_b_)
     {return +(caml_compare_val_cw_(x_a_,y_b_,false)!=0)}
    function caml_new_string_eS_(x_a_){return new MlString_w_(x_a_)}
    function caml_mul_eQ_(x_a_,y_b_)
     {return ((x_a_>>16)*y_b_<<16)+(x_a_&_aw_)*y_b_|0}
    function caml_mod_eP_(x_a_,y_b_)
     {if(y_b_==0)caml_raise_zero_divide_eX_();return x_a_%y_b_}
    function caml_raise_zero_divide_eX_()
     {caml_raise_constant_eV_(caml_global_data_aC_[6])}
    function caml_raise_constant_eV_(tag_a_){throw [0,tag_a_]}
    function caml_ml_output_eO_(oc_a_,buffer_b_,offset_c_,len_d_)
     {var string_f_;
      if(offset_c_==0&&buffer_b_.getLen()==len_d_)
       string_f_=buffer_b_;
      else
       {string_f_=caml_create_string_cx_(len_d_);
        caml_blit_string_cv_(buffer_b_,offset_c_,string_f_,0,len_d_)}
      var
       jsstring_e_=string_f_.toString(),
       id_g_=jsstring_e_.lastIndexOf("\n");
      if(id_g_<0)
       caml_ml_output_buffer_O_+=jsstring_e_;
      else
       {caml_ml_output_buffer_O_+=jsstring_e_.substr(0,id_g_);
        caml_ml_flush_cA_(oc_a_);
        caml_ml_output_buffer_O_+=jsstring_e_.substr(id_g_+1)}}
    function caml_ml_out_channels_list_eN_(){return 0}
    function caml_ml_open_descriptor_out_eM_(x_a_){return x_a_}
    function caml_ml_flush_cA_(oc_a_)
     {joo_global_object_c_.console&&
      joo_global_object_c_.console.log&&
      caml_ml_output_buffer_O_!=
      _i_&&
      joo_global_object_c_.console.log(caml_ml_output_buffer_O_);
      caml_ml_output_buffer_O_=_i_}
    var
     caml_ml_output_buffer_O_=_i_,
     caml_md5_string_eL_=
      function()
        {function add_m_(x_a_,y_b_){return x_a_+y_b_|0}
         function xx_l_(q_a_,a_b_,b_c_,x_d_,s_e_,t_f_)
          {a_b_=add_m_(add_m_(a_b_,q_a_),add_m_(x_d_,t_f_));
           return add_m_(a_b_<<s_e_|a_b_>>>32-s_e_,b_c_)}
         function ff_h_(a_a_,b_b_,c_c_,d_d_,x_e_,s_f_,t_g_)
          {return xx_l_(b_b_&c_c_|~b_b_&d_d_,a_a_,b_b_,x_e_,s_f_,t_g_)}
         function gg_i_(a_a_,b_b_,c_c_,d_d_,x_e_,s_f_,t_g_)
          {return xx_l_(b_b_&d_d_|c_c_&~d_d_,a_a_,b_b_,x_e_,s_f_,t_g_)}
         function hh_j_(a_a_,b_b_,c_c_,d_d_,x_e_,s_f_,t_g_)
          {return xx_l_(b_b_^c_c_^d_d_,a_a_,b_b_,x_e_,s_f_,t_g_)}
         function ii_k_(a_a_,b_b_,c_c_,d_d_,x_e_,s_f_,t_g_)
          {return xx_l_(c_c_^(b_b_|~d_d_),a_a_,b_b_,x_e_,s_f_,t_g_)}
         function md5_n_(buffer_a_,length_b_)
          {var i_c_=length_b_;
           buffer_a_[i_c_>>2]|=_cr_<<8*(i_c_&3);
           for(i_c_=(i_c_&~3)+4;(i_c_&63)<56;i_c_+=4)buffer_a_[i_c_>>2]=0;
           buffer_a_[i_c_>>2]=length_b_<<3;
           i_c_+=4;
           buffer_a_[i_c_>>2]=length_b_>>29&536870911;
           var w_l_=[1732584193,4023233417,2562383102,271733878];
           for(i_c_=0;i_c_<buffer_a_.length;i_c_+=16)
            {var a_d_=w_l_[0],b_e_=w_l_[1],c_f_=w_l_[2],d_g_=w_l_[3];
             a_d_=ff_h_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+0],7,3614090360);
             d_g_=ff_h_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+1],12,3905402710);
             c_f_=ff_h_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+2],17,606105819);
             b_e_=ff_h_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+3],22,3250441966);
             a_d_=ff_h_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+4],7,4118548399);
             d_g_=ff_h_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+5],12,1200080426);
             c_f_=ff_h_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+6],17,2821735955);
             b_e_=ff_h_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+7],22,4249261313);
             a_d_=ff_h_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+8],7,1770035416);
             d_g_=ff_h_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+9],12,2336552879);
             c_f_=ff_h_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+10],17,4294925233);
             b_e_=ff_h_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+11],22,2304563134);
             a_d_=ff_h_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+12],7,1804603682);
             d_g_=ff_h_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+13],12,4254626195);
             c_f_=ff_h_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+14],17,2792965006);
             b_e_=ff_h_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+15],22,1236535329);
             a_d_=gg_i_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+1],5,4129170786);
             d_g_=gg_i_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+6],9,3225465664);
             c_f_=gg_i_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+11],14,643717713);
             b_e_=gg_i_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+0],20,3921069994);
             a_d_=gg_i_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+5],5,3593408605);
             d_g_=gg_i_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+10],9,38016083);
             c_f_=gg_i_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+15],14,3634488961);
             b_e_=gg_i_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+4],20,3889429448);
             a_d_=gg_i_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+9],5,568446438);
             d_g_=gg_i_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+14],9,3275163606);
             c_f_=gg_i_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+3],14,4107603335);
             b_e_=gg_i_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+8],20,1163531501);
             a_d_=gg_i_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+13],5,2850285829);
             d_g_=gg_i_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+2],9,4243563512);
             c_f_=gg_i_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+7],14,1735328473);
             b_e_=gg_i_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+12],20,2368359562);
             a_d_=hh_j_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+5],4,4294588738);
             d_g_=hh_j_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+8],11,2272392833);
             c_f_=hh_j_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+11],16,1839030562);
             b_e_=hh_j_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+14],23,4259657740);
             a_d_=hh_j_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+1],4,2763975236);
             d_g_=hh_j_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+4],11,1272893353);
             c_f_=hh_j_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+7],16,4139469664);
             b_e_=hh_j_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+10],23,3200236656);
             a_d_=hh_j_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+13],4,681279174);
             d_g_=hh_j_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+0],11,3936430074);
             c_f_=hh_j_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+3],16,3572445317);
             b_e_=hh_j_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+6],23,76029189);
             a_d_=hh_j_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+9],4,3654602809);
             d_g_=hh_j_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+12],11,3873151461);
             c_f_=hh_j_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+15],16,530742520);
             b_e_=hh_j_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+2],23,3299628645);
             a_d_=ii_k_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+0],6,4096336452);
             d_g_=ii_k_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+7],10,1126891415);
             c_f_=ii_k_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+14],15,2878612391);
             b_e_=ii_k_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+5],21,4237533241);
             a_d_=ii_k_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+12],6,1700485571);
             d_g_=ii_k_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+3],10,2399980690);
             c_f_=ii_k_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+10],15,4293915773);
             b_e_=ii_k_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+1],21,2240044497);
             a_d_=ii_k_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+8],6,1873313359);
             d_g_=ii_k_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+15],10,4264355552);
             c_f_=ii_k_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+6],15,2734768916);
             b_e_=ii_k_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+13],21,1309151649);
             a_d_=ii_k_(a_d_,b_e_,c_f_,d_g_,buffer_a_[i_c_+4],6,4149444226);
             d_g_=ii_k_(d_g_,a_d_,b_e_,c_f_,buffer_a_[i_c_+11],10,3174756917);
             c_f_=ii_k_(c_f_,d_g_,a_d_,b_e_,buffer_a_[i_c_+2],15,718787259);
             b_e_=ii_k_(b_e_,c_f_,d_g_,a_d_,buffer_a_[i_c_+9],21,3951481745);
             w_l_[0]=add_m_(a_d_,w_l_[0]);
             w_l_[1]=add_m_(b_e_,w_l_[1]);
             w_l_[2]=add_m_(c_f_,w_l_[2]);
             w_l_[3]=add_m_(d_g_,w_l_[3])}
           var t_o_=[];
           for(var i_c_=0;i_c_<4;i_c_++)
            for(var j_n_=0;j_n_<4;j_n_++)
             t_o_[i_c_*4+j_n_]=w_l_[i_c_]>>8*j_n_&_p_;
           return t_o_}
         return function(s_a_,ofs_b_,len_c_)
          {var buf_h_=[];
           if(s_a_.array)
            {var a_f_=s_a_.array;
             for(var i_d_=0;i_d_<len_c_;i_d_+=4)
              {var j_e_=i_d_+ofs_b_;
               buf_h_[i_d_>>2]=
               a_f_[j_e_]|
               a_f_[j_e_+1]<<
               8|
               a_f_[j_e_+2]<<
               16|
               a_f_[j_e_+3]<<
               24}
             for(;i_d_<len_c_;i_d_++)
              buf_h_[i_d_>>2]|=a_f_[i_d_+ofs_b_]<<8*(i_d_&3)}
           else
            {var b_g_=s_a_.getFullBytes();
             for(var i_d_=0;i_d_<len_c_;i_d_+=4)
              {var j_e_=i_d_+ofs_b_;
               buf_h_[i_d_>>2]=
               b_g_.charCodeAt(j_e_)|
               b_g_.charCodeAt(j_e_+1)<<
               8|
               b_g_.charCodeAt(j_e_+2)<<
               16|
               b_g_.charCodeAt(j_e_+3)<<
               24}
             for(;i_d_<len_c_;i_d_++)
              buf_h_[i_d_>>2]|=b_g_.charCodeAt(i_d_+ofs_b_)<<8*(i_d_&3)}
           return new MlStringFromArray_cu_(md5_n_(buf_h_,len_c_))}}
       ();
    function caml_make_vect_eK_(len_a_,init_b_)
     {var b_d_=[0];
      for(var i_c_=1;i_c_<=len_a_;i_c_++)b_d_[i_c_]=init_b_;
      return b_d_}
    function caml_lessthan_eJ_(x_a_,y_b_)
     {return +(caml_compare_aA_(x_a_,y_b_,false)<0)}
    function caml_lessequal_eI_(x_a_,y_b_)
     {return +(caml_compare_aA_(x_a_,y_b_,false)<=0)}
    function caml_js_wrap_callback_eH_(f_a_)
     {var toArray_c_=Array.prototype.slice;
      return function()
       {var args_b_=arguments.length>0?toArray_c_.call(arguments):[undefined];
        return caml_call_gen_y_(f_a_,args_b_)}}
    function caml_js_pure_expr_eG_(f_a_){return f_a_()}
    function caml_js_get_console_eF_()
     {var
       c_b_=joo_global_object_c_.console?joo_global_object_c_.console:{},
       m_d_=
        ["log",
         "debug",
         "info",
         "warn",
         "error",
         "assert",
         "dir",
         "dirxml",
         "trace",
         "group",
         "groupCollapsed",
         "groupEnd",
         "time",
         "timeEnd"];
      function f_e_(){}
      for(var i_a_=0;i_a_<m_d_.length;i_a_++)
       if(!c_b_[m_d_[i_a_]])c_b_[m_d_[i_a_]]=f_e_;
      return c_b_}
    function caml_js_from_array_eE_(a_a_){return a_a_.slice(1)}
    function caml_is_printable_eD_(c_a_){return +(c_a_>31&&c_a_<127)}
    function caml_int_of_string_eC_(s_a_)
     {var
       r_g_=caml_parse_sign_and_base_eU_(s_a_),
       i_e_=r_g_[0],
       sign_h_=r_g_[1],
       base_f_=r_g_[2],
       threshold_i_=-1>>>0,
       c_d_=s_a_.get(i_e_),
       d_c_=caml_parse_digit_cB_(c_d_);
      if(d_c_<0||d_c_>=base_f_)caml_failwith_aB_(_ar_);
      var res_b_=d_c_;
      for(;;)
       {i_e_++;
        c_d_=s_a_.get(i_e_);
        if(c_d_==95)continue;
        d_c_=caml_parse_digit_cB_(c_d_);
        if(d_c_<0||d_c_>=base_f_)break;
        res_b_=base_f_*res_b_+d_c_;
        if(res_b_>threshold_i_)caml_failwith_aB_(_ar_)}
      if(i_e_!=s_a_.getLen())caml_failwith_aB_(_ar_);
      res_b_=sign_h_*res_b_;
      if((res_b_|0)!=res_b_)caml_failwith_aB_(_ar_);
      return res_b_}
    function caml_failwith_aB_(msg_a_)
     {caml_raise_with_string_cC_(caml_global_data_aC_[3],msg_a_)}
    var caml_global_data_aC_=[0];
    function caml_parse_digit_cB_(c_a_)
     {if(c_a_>=48&&c_a_<=57)return c_a_-48;
      if(c_a_>=65&&c_a_<=90)return c_a_-55;
      if(c_a_>=97&&c_a_<=122)return c_a_-87;
      return -1}
    function caml_parse_sign_and_base_eU_(s_a_)
     {var i_b_=0,base_c_=10,sign_d_=s_a_.get(0)==45?(i_b_++,-1):1;
      if(s_a_.get(i_b_)==48)
       switch(s_a_.get(i_b_+1))
        {case _a8_:
         case 88:base_c_=16;i_b_+=2;break;
         case _au_:
         case 79:base_c_=8;i_b_+=2;break;
         case 98:
         case 66:base_c_=2;i_b_+=2;break
         }
      return [i_b_,sign_d_,base_c_]}
    function caml_int64_format_es_(fmt_a_,x_b_)
     {var f_c_=caml_parse_format_bg_(fmt_a_);
      if(f_c_.signedconv&&caml_int64_is_negative_et_(x_b_))
       {f_c_.sign=-1;x_b_=caml_int64_neg_ew_(x_b_)}
      var
       buffer_d_=_i_,
       wbase_h_=caml_int64_of_int32_ex_(f_c_.base),
       cvtbl_g_="0123456789abcdef";
      do
       {var p_f_=caml_int64_udivmod_eA_(x_b_,wbase_h_);
        x_b_=p_f_[1];
        buffer_d_=cvtbl_g_.charAt(caml_int64_to_int32_ez_(p_f_[2]))+buffer_d_}
      while
       (!caml_int64_is_zero_eu_(x_b_));
      if(f_c_.prec>=0)
       {f_c_.filler=_r_;
        var n_e_=f_c_.prec-buffer_d_.length;
        if(n_e_>0)buffer_d_=caml_str_repeat_aa_(n_e_,_m_)+buffer_d_}
      return caml_finish_formatting_be_(f_c_,buffer_d_)}
    function caml_int64_neg_ew_(x_a_)
     {var
       y1_b_=-x_a_[1],
       y2_c_=-x_a_[2]+(y1_b_>>24),
       y3_d_=-x_a_[3]+(y2_c_>>24);
      return [_p_,y1_b_&_q_,y2_c_&_q_,y3_d_&_aw_]}
    function caml_int64_is_negative_et_(x_a_){return x_a_[3]<<16<0}
    function caml_int64_to_int32_ez_(x_a_){return x_a_[1]|x_a_[2]<<24}
    function caml_int64_udivmod_eA_(x_a_,y_b_)
     {var
       offset_e_=0,
       modulus_d_=x_a_.slice(),
       divisor_c_=y_b_.slice(),
       quotient_f_=[_p_,0,0,0];
      while(caml_int64_ucompare_cz_(modulus_d_,divisor_c_)>0)
       {offset_e_++;caml_int64_lsl1_cy_(divisor_c_)}
      while(offset_e_>=0)
       {offset_e_--;
        caml_int64_lsl1_cy_(quotient_f_);
        if(caml_int64_ucompare_cz_(modulus_d_,divisor_c_)>=0)
         {quotient_f_[1]++;
          modulus_d_=caml_int64_sub_ey_(modulus_d_,divisor_c_)}
        caml_int64_lsr1_ev_(divisor_c_)}
      return [0,quotient_f_,modulus_d_]}
    function caml_int64_lsr1_ev_(x_a_)
     {x_a_[1]=(x_a_[1]>>>1|x_a_[2]<<23)&_q_;
      x_a_[2]=(x_a_[2]>>>1|x_a_[3]<<23)&_q_;
      x_a_[3]=x_a_[3]>>>1}
    function caml_int64_lsl1_cy_(x_a_)
     {x_a_[3]=x_a_[3]<<1|x_a_[2]>>23;
      x_a_[2]=(x_a_[2]<<1|x_a_[1]>>23)&_q_;
      x_a_[1]=x_a_[1]<<1&_q_}
    function caml_int64_ucompare_cz_(x_a_,y_b_)
     {if(x_a_[3]>y_b_[3])return 1;
      if(x_a_[3]<y_b_[3])return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_int64_sub_ey_(x_a_,y_b_)
     {var
       z1_c_=x_a_[1]-y_b_[1],
       z2_d_=x_a_[2]-y_b_[2]+(z1_c_>>24),
       z3_e_=x_a_[3]-y_b_[3]+(z2_d_>>24);
      return [_p_,z1_c_&_q_,z2_d_&_q_,z3_e_&_aw_]}
    function caml_int64_of_int32_ex_(x_a_)
     {return [_p_,x_a_&_q_,x_a_>>24&_q_,x_a_>>31&_aw_]}
    function caml_int64_is_zero_eu_(x_a_){return (x_a_[3]|x_a_[2]|x_a_[1])==0}
    function caml_greaterthan_eq_(x_a_,y_b_)
     {return +(caml_compare_aA_(x_a_,y_b_,false)>0)}
    function caml_greaterequal_ep_(x_a_,y_b_)
     {return +(caml_compare_aA_(x_a_,y_b_,false)>=0)}
    function caml_compare_aA_(a_a_,b_b_)
     {return caml_compare_val_cw_(a_a_,b_b_,true)}
    function caml_compare_val_cw_(a_a_,b_b_,total_c_)
     {var stack_e_=[];
      for(;;)
       {if(!(total_c_&&a_a_===b_b_))
         {if(a_a_ instanceof MlString_w_)
           {if(b_b_ instanceof MlString_w_)
             {if(a_a_!=b_b_)
               {var x_d_=a_a_.compare(b_b_);if(x_d_!=0)return x_d_}}
            else
             return 1}
          else
           if(a_a_ instanceof Array&&a_a_[0]===(a_a_[0]|0))
            {var ta_g_=a_a_[0];
             if(ta_g_===_co_)
              {a_a_=a_a_[1];continue}
             else
              if(b_b_ instanceof Array&&b_b_[0]===(b_b_[0]|0))
               {var tb_h_=b_b_[0];
                if(tb_h_===_co_)
                 {b_b_=b_b_[1];continue}
                else
                 if(ta_g_!=tb_h_)
                  {return ta_g_<tb_h_?-1:1}
                 else
                  {switch(ta_g_)
                    {case 248:
                      {var x_d_=caml_int_compare_eB_(a_a_[2],b_b_[2]);
                       if(x_d_!=0)return x_d_;
                       break}
                     case _p_:
                      {var x_d_=caml_int64_compare_er_(a_a_,b_b_);
                       if(x_d_!=0)return x_d_;
                       break}
                     default:
                      if(a_a_.length!=b_b_.length)
                       return a_a_.length<b_b_.length?-1:1;
                      if(a_a_.length>1)stack_e_.push(a_a_,b_b_,1)}}}
              else
               return 1}
           else
            if
             (b_b_ instanceof MlString_w_||
              b_b_ instanceof Array&&
              b_b_[0]===
              (b_b_[0]|0))
             {return -1}
            else
             {if(a_a_<b_b_)return -1;
              if(a_a_>b_b_)return 1;
              if(total_c_&&a_a_!=b_b_)
               {if(a_a_==a_a_)return 1;if(b_b_==b_b_)return -1}}}
        if(stack_e_.length==0)return 0;
        var i_f_=stack_e_.pop();
        b_b_=stack_e_.pop();
        a_a_=stack_e_.pop();
        if(i_f_+1<a_a_.length)stack_e_.push(a_a_,b_b_,i_f_+1);
        a_a_=a_a_[i_f_];
        b_b_=b_b_[i_f_]}}
    function caml_int_compare_eB_(a_a_,b_b_)
     {if(a_a_<b_b_)return -1;if(a_a_==b_b_)return 0;return 1}
    function caml_int64_compare_er_(x_a_,y_b_)
     {var x3_c_=x_a_[3]<<16,y3_d_=y_b_[3]<<16;
      if(x3_c_>y3_d_)return 1;
      if(x3_c_<y3_d_)return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_format_int_eo_(fmt_a_,i_b_)
     {if(fmt_a_.toString()=="%d")return new MlWrappedString_$_(_i_+i_b_);
      var f_c_=caml_parse_format_bg_(fmt_a_);
      if(i_b_<0){if(f_c_.signedconv){f_c_.sign=-1;i_b_=-i_b_}else i_b_>>>=0}
      var s_d_=i_b_.toString(f_c_.base);
      if(f_c_.prec>=0)
       {f_c_.filler=_r_;
        var n_e_=f_c_.prec-s_d_.length;
        if(n_e_>0)s_d_=caml_str_repeat_aa_(n_e_,_m_)+s_d_}
      return caml_finish_formatting_be_(f_c_,s_d_)}
    function caml_format_float_en_(fmt_a_,x_b_)
     {var
       s_c_,
       f_f_=caml_parse_format_bg_(fmt_a_),
       prec_e_=f_f_.prec<0?6:f_f_.prec;
      if(x_b_<0){f_f_.sign=-1;x_b_=-x_b_}
      if(isNaN(x_b_))
       {s_c_=_cl_;f_f_.filler=_r_}
      else
       if(!isFinite(x_b_))
        {s_c_="inf";f_f_.filler=_r_}
       else
        switch(f_f_.conv)
         {case _Y_:
           var s_c_=x_b_.toExponential(prec_e_),i_d_=s_c_.length;
           if(s_c_.charAt(i_d_-3)==_Y_)
            s_c_=s_c_.slice(0,i_d_-1)+_m_+s_c_.slice(i_d_-1);
           break;
          case _a5_:s_c_=x_b_.toFixed(prec_e_);break;
          case _ch_:
           prec_e_=prec_e_?prec_e_:1;
           s_c_=x_b_.toExponential(prec_e_-1);
           var j_i_=s_c_.indexOf(_Y_),exp_h_=+s_c_.slice(j_i_+1);
           if(exp_h_<-4||x_b_.toFixed(0).length>prec_e_)
            {var i_d_=j_i_-1;
             while(s_c_.charAt(i_d_)==_m_)i_d_--;
             if(s_c_.charAt(i_d_)==_ax_)i_d_--;
             s_c_=s_c_.slice(0,i_d_+1)+s_c_.slice(j_i_);
             i_d_=s_c_.length;
             if(s_c_.charAt(i_d_-3)==_Y_)
              s_c_=s_c_.slice(0,i_d_-1)+_m_+s_c_.slice(i_d_-1);
             break}
           else
            {var p_g_=prec_e_;
             if(exp_h_<0)
              {p_g_-=exp_h_+1;s_c_=x_b_.toFixed(p_g_)}
             else
              while(s_c_=x_b_.toFixed(p_g_),s_c_.length>prec_e_+1)p_g_--;
             if(p_g_)
              {var i_d_=s_c_.length-1;
               while(s_c_.charAt(i_d_)==_m_)i_d_--;
               if(s_c_.charAt(i_d_)==_ax_)i_d_--;
               s_c_=s_c_.slice(0,i_d_+1)}}
           break
          }
      return caml_finish_formatting_be_(f_f_,s_c_)}
    function caml_finish_formatting_be_(f_a_,rawbuffer_b_)
     {if(f_a_.uppercase)rawbuffer_b_=rawbuffer_b_.toUpperCase();
      var len_e_=rawbuffer_b_.length;
      if(f_a_.signedconv&&(f_a_.sign<0||f_a_.signstyle!=_H_))len_e_++;
      if(f_a_.alternate){if(f_a_.base==8)len_e_+=1;if(f_a_.base==16)len_e_+=2}
      var buffer_c_=_i_;
      if(f_a_.justify==_av_&&f_a_.filler==_r_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=_r_;
      if(f_a_.signedconv)
       {if(f_a_.sign<0)
         buffer_c_+=_H_;
        else
         if(f_a_.signstyle!=_H_)buffer_c_+=f_a_.signstyle}
      if(f_a_.alternate&&f_a_.base==8)buffer_c_+=_m_;
      if(f_a_.alternate&&f_a_.base==16)buffer_c_+="0x";
      if(f_a_.justify==_av_&&f_a_.filler==_m_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=_m_;
      buffer_c_+=rawbuffer_b_;
      if(f_a_.justify==_H_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=_r_;
      return new MlWrappedString_$_(buffer_c_)}
    function caml_parse_format_bg_(fmt_a_)
     {fmt_a_=fmt_a_.toString();
      var len_e_=fmt_a_.length;
      if(len_e_>31)caml_invalid_argument_bf_("format_int: format too long");
      var
       f_b_=
        {justify:_av_,
         signstyle:_H_,
         filler:_r_,
         alternate:false,
         base:0,
         signedconv:false,
         width:0,
         uppercase:false,
         sign:1,
         prec:-1,
         conv:_a5_};
      for(var i_d_=0;i_d_<len_e_;i_d_++)
       {var c_c_=fmt_a_.charAt(i_d_);
        switch(c_c_)
         {case _H_:f_b_.justify=_H_;break;
          case _av_:
          case _r_:f_b_.signstyle=c_c_;break;
          case _m_:f_b_.filler=_m_;break;
          case "#":f_b_.alternate=true;break;
          case "1":
          case "2":
          case "3":
          case "4":
          case "5":
          case "6":
          case "7":
          case "8":
          case "9":
           f_b_.width=0;
           while(c_c_=fmt_a_.charCodeAt(i_d_)-48,c_c_>=0&&c_c_<=9)
            {f_b_.width=f_b_.width*10+c_c_;i_d_++}
           i_d_--;
           break;
          case _ax_:
           f_b_.prec=0;
           i_d_++;
           while(c_c_=fmt_a_.charCodeAt(i_d_)-48,c_c_>=0&&c_c_<=9)
            {f_b_.prec=f_b_.prec*10+c_c_;i_d_++}
           i_d_--;
          case "d":
          case "i":f_b_.signedconv=true;
          case "u":f_b_.base=10;break;
          case _ci_:f_b_.base=16;break;
          case "X":f_b_.base=16;f_b_.uppercase=true;break;
          case "o":f_b_.base=8;break;
          case _Y_:
          case _a5_:
          case _ch_:f_b_.signedconv=true;f_b_.conv=c_c_;break;
          case "E":
          case "F":
          case "G":
           f_b_.signedconv=true;
           f_b_.uppercase=true;
           f_b_.conv=c_c_.toLowerCase();
           break
          }}
      return f_b_}
    function caml_fill_string_em_(s_a_,i_b_,l_c_,c_d_)
     {s_a_.fill(i_b_,l_c_,c_d_)}
    function caml_create_string_cx_(len_a_)
     {if(len_a_<0)caml_invalid_argument_bf_("String.create");
      return new MlMakeString_ct_(len_a_)}
    function caml_classify_float_el_(x_a_)
     {if(isFinite(x_a_))
       {if(Math.abs(x_a_)>=2.22507385850720138e-308)return 0;
        if(x_a_!=0)return 1;
        return 2}
      return isNaN(x_a_)?4:3}
    function caml_call_gen_y_(f_c_,args_b_)
     {if(f_c_.fun)return caml_call_gen_y_(f_c_.fun,args_b_);
      var n_a_=f_c_.length,d_d_=n_a_-args_b_.length;
      if(d_d_==0)
       return f_c_.apply(null,args_b_);
      else
       if(d_d_<0)
        return caml_call_gen_y_
                (f_c_.apply(null,args_b_.slice(0,n_a_)),args_b_.slice(n_a_));
       else
        return function(x_a_)
         {return caml_call_gen_y_(f_c_,args_b_.concat([x_a_]))}}
    function caml_blit_string_cv_(s1_a_,i1_b_,s2_c_,i2_d_,len_e_)
     {if(len_e_===0)return;
      if(i2_d_===s2_c_.last&&s2_c_.bytes!=null)
       {var b_f_=s1_a_.bytes;
        if(b_f_==null)b_f_=s1_a_.toBytes();
        if(i1_b_>0||s1_a_.last>len_e_)b_f_=b_f_.slice(i1_b_,i1_b_+len_e_);
        s2_c_.bytes+=b_f_;
        s2_c_.last+=b_f_.length;
        return}
      var a_g_=s2_c_.array;
      if(!a_g_)a_g_=s2_c_.toArray();else{s2_c_.bytes=s2_c_.string=null}
      s1_a_.blitToArray(i1_b_,a_g_,i2_d_,len_e_)}
    function caml_array_set_ek_(array_a_,index_b_,newval_c_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_az_();
      array_a_[index_b_+1]=newval_c_;
      return 0}
    function caml_array_get_ej_(array_a_,index_b_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_az_();
      return array_a_[index_b_+1]}
    function caml_str_repeat_aa_(n_a_,s_b_)
     {if(!n_a_){return _i_}
      if(n_a_&1){return caml_str_repeat_aa_(n_a_-1,s_b_)+s_b_}
      var r_c_=caml_str_repeat_aa_(n_a_>>1,s_b_);
      return r_c_+r_c_}
    function MlString_w_(param_a_)
     {if(param_a_!=null)
       {this.bytes=this.fullBytes=param_a_;this.last=this.len=param_a_.length}}
    MlString_w_.prototype=
    {string:null,
     bytes:null,
     fullBytes:null,
     array:null,
     len:null,
     last:0,
     toJsString:
     function()
      {var a_a_=this.getFullBytes();
       try
        {return this.string=decodeURIComponent(escape(a_a_))}
       catch(e_f_)
        {joo_global_object_c_.console&&
         joo_global_object_c_.console.error&&
         joo_global_object_c_.console.error
          ('MlString.toJsString: wrong encoding for \"%s\" ',a_a_);
         return a_a_}},
     toBytes:
     function()
      {if(this.string!=null)
        {try
          {var b_a_=unescape(encodeURIComponent(this.string))}
         catch(e_f_)
          {joo_global_object_c_.console&&
           joo_global_object_c_.console.error&&
           joo_global_object_c_.console.error
            ('MlString.toBytes: wrong encoding for \"%s\" ',this.string);
           var b_a_=this.string}}
       else
        {var b_a_=_i_,a_d_=this.array,l_e_=a_d_.length;
         for(var i_b_=0;i_b_<l_e_;i_b_++)b_a_+=String.fromCharCode(a_d_[i_b_])}
       this.bytes=this.fullBytes=b_a_;
       this.last=this.len=b_a_.length;
       return b_a_},
     getBytes:
     function()
      {var b_a_=this.bytes;if(b_a_==null)b_a_=this.toBytes();return b_a_},
     getFullBytes:
     function()
      {var b_a_=this.fullBytes;
       if(b_a_!==null)return b_a_;
       b_a_=this.bytes;
       if(b_a_==null)b_a_=this.toBytes();
       if(this.last<this.len)
        {this.bytes=b_a_+=caml_str_repeat_aa_(this.len-this.last,"\0");
         this.last=this.len}
       this.fullBytes=b_a_;
       return b_a_},
     toArray:
     function()
      {var b_c_=this.bytes;
       if(b_c_==null)b_c_=this.toBytes();
       var a_b_=[],l_d_=this.last;
       for(var i_a_=0;i_a_<l_d_;i_a_++)a_b_[i_a_]=b_c_.charCodeAt(i_a_);
       for(l_d_=this.len;i_a_<l_d_;i_a_++)a_b_[i_a_]=0;
       this.string=this.bytes=this.fullBytes=null;
       this.last=this.len;
       this.array=a_b_;
       return a_b_},
     getArray:
     function(){var a_a_=this.array;if(!a_a_)a_a_=this.toArray();return a_a_},
     getLen:
     function()
      {var len_a_=this.len;
       if(len_a_!==null)return len_a_;
       this.toBytes();
       return this.len},
     toString:
     function(){var s_a_=this.string;return s_a_?s_a_:this.toJsString()},
     valueOf:
     function(){var s_a_=this.string;return s_a_?s_a_:this.toJsString()},
     blitToArray:
     function(i1_a_,a2_b_,i2_c_,l_d_)
      {var a1_g_=this.array;
       if(a1_g_)
        {if(i2_c_<=i1_a_)
          {for(var i_e_=0;i_e_<l_d_;i_e_++)a2_b_[i2_c_+i_e_]=a1_g_[i1_a_+i_e_]}
         else
          {for(var i_e_=l_d_-1;i_e_>=0;i_e_--)
            a2_b_[i2_c_+i_e_]=a1_g_[i1_a_+i_e_]}}
       else
        {var b_f_=this.bytes;
         if(b_f_==null)b_f_=this.toBytes();
         var l1_h_=this.last-i1_a_;
         if(l_d_<=l1_h_)
          for(var i_e_=0;i_e_<l_d_;i_e_++)
           a2_b_[i2_c_+i_e_]=b_f_.charCodeAt(i1_a_+i_e_);
         else
          {for(var i_e_=0;i_e_<l1_h_;i_e_++)
            a2_b_[i2_c_+i_e_]=b_f_.charCodeAt(i1_a_+i_e_);
           for(;i_e_<l_d_;i_e_++)a2_b_[i2_c_+i_e_]=0}}},
     get:
     function(i_a_)
      {var a_c_=this.array;
       if(a_c_)return a_c_[i_a_];
       var b_b_=this.bytes;
       if(b_b_==null)b_b_=this.toBytes();
       return i_a_<this.last?b_b_.charCodeAt(i_a_):0},
     safeGet:
     function(i_a_)
      {if(this.len==null)this.toBytes();
       if(i_a_<0||i_a_>=this.len)caml_array_bound_error_az_();
       return this.get(i_a_)},
     set:
     function(i_a_,c_b_)
      {var a_c_=this.array;
       if(!a_c_)
        {if(this.last==i_a_)
          {this.bytes+=String.fromCharCode(c_b_&_p_);this.last++;return 0}
         a_c_=this.toArray()}
       else
        if(this.bytes!=null){this.bytes=this.fullBytes=this.string=null}
       a_c_[i_a_]=c_b_&_p_;
       return 0},
     safeSet:
     function(i_a_,c_b_)
      {if(this.len==null)this.toBytes();
       if(i_a_<0||i_a_>=this.len)caml_array_bound_error_az_();
       this.set(i_a_,c_b_)},
     fill:
     function(ofs_a_,len_b_,c_c_)
      {if(ofs_a_>=this.last&&this.last&&c_c_==0)return;
       var a_d_=this.array;
       if(!a_d_)
        a_d_=this.toArray();
       else
        if(this.bytes!=null){this.bytes=this.fullBytes=this.string=null}
       var l_f_=ofs_a_+len_b_;
       for(var i_e_=ofs_a_;i_e_<l_f_;i_e_++)a_d_[i_e_]=c_c_},
     compare:
     function(s2_a_)
      {if(this.string!=null&&s2_a_.string!=null)
        {if(this.string<s2_a_.string)return -1;
         if(this.string>s2_a_.string)return 1;
         return 0}
       var b1_b_=this.getFullBytes(),b2_c_=s2_a_.getFullBytes();
       if(b1_b_<b2_c_)return -1;
       if(b1_b_>b2_c_)return 1;
       return 0},
     equal:
     function(s2_a_)
      {if(this.string!=null&&s2_a_.string!=null)
        return this.string==s2_a_.string;
       return this.getFullBytes()==s2_a_.getFullBytes()},
     lessThan:
     function(s2_a_)
      {if(this.string!=null&&s2_a_.string!=null)
        return this.string<s2_a_.string;
       return this.getFullBytes()<s2_a_.getFullBytes()},
     lessEqual:
     function(s2_a_)
      {if(this.string!=null&&s2_a_.string!=null)
        return this.string<=s2_a_.string;
       return this.getFullBytes()<=s2_a_.getFullBytes()}};
    function MlWrappedString_$_(s_a_){this.string=s_a_}
    MlWrappedString_$_.prototype=new MlString_w_();
    function MlMakeString_ct_(l_a_){this.bytes=_i_;this.len=l_a_}
    MlMakeString_ct_.prototype=new MlString_w_();
    function MlStringFromArray_cu_(a_a_)
     {var len_b_=a_a_.length;this.array=a_a_;this.len=this.last=len_b_}
    MlStringFromArray_cu_.prototype=new MlString_w_();
    function caml_array_bound_error_az_()
     {caml_invalid_argument_bf_("index out of bounds")}
    function caml_invalid_argument_bf_(msg_a_)
     {caml_raise_with_string_cC_(caml_global_data_aC_[4],msg_a_)}
    function caml_raise_with_string_cC_(tag_a_,msg_b_)
     {caml_raise_with_arg_eW_(tag_a_,new MlWrappedString_$_(msg_b_))}
    function caml_raise_with_arg_eW_(tag_a_,arg_b_){throw [0,tag_a_,arg_b_]}
    var
     _a_=_i_,
     _bX_='"',
     _ao_="'",
     _bY_=_ax_,
     _b__="20px",
     _b0_=">",
     _bZ_="bad tag name specification",
     _b2_="color",
     _ei_="false",
     _b1_="p",
     _b3_="polygons.ml",
     _b$_="scale",
     _cc_="transform",
     _eh_="true",
     caml_array_get_d_=caml_array_get_ej_,
     caml_array_set_g_=caml_array_set_ek_,
     caml_blit_string_F_=caml_blit_string_cv_,
     caml_create_string_x_=caml_create_string_cx_,
     caml_format_float_b6_=caml_format_float_en_,
     caml_format_int_aq_=caml_format_int_eo_,
     caml_greaterequal_b9_=caml_greaterequal_ep_,
     caml_greaterthan_ca_=caml_greaterthan_eq_,
     caml_int_of_string_b7_=caml_int_of_string_eC_,
     caml_is_printable_a2_=caml_is_printable_eD_,
     caml_js_from_array_G_=caml_js_from_array_eE_,
     caml_js_wrap_callback_cb_=caml_js_wrap_callback_eH_,
     caml_lessthan_a3_=caml_lessthan_eJ_,
     caml_make_vect_k_=caml_make_vect_eK_,
     caml_ml_flush_b5_=caml_ml_flush_cA_,
     caml_ml_open_descriptor_out_b4_=caml_ml_open_descriptor_out_eM_,
     caml_mul_a1_=caml_mul_eQ_,
     caml_new_string_b_=caml_new_string_eS_,
     caml_register_global_ap_=caml_register_global_eY_;
    function caml_call_gen1_h_(_a_,_b_)
     {return _a_.length==1?_a_(_b_):caml_call_gen_y_(_a_,[_b_])}
    function caml_call_gen2_n_(_a_,_b_,_c_)
     {return _a_.length==2?_a_(_b_,_c_):caml_call_gen_y_(_a_,[_b_,_c_])}
    function caml_call_gen3_j_(_a_,_b_,_c_,_d_)
     {return _a_.length==3
              ?_a_(_b_,_c_,_d_)
              :caml_call_gen_y_(_a_,[_b_,_c_,_d_])}
    function caml_call_gen4_b8_(_a_,_b_,_c_,_d_,_e_)
     {return _a_.length==4
              ?_a_(_b_,_c_,_d_,_e_)
              :caml_call_gen_y_(_a_,[_b_,_c_,_d_,_e_])}
    var
     _ab_=[0,caml_new_string_b_("Failure")],
     _bh_=[0,caml_new_string_b_("Invalid_argument")],
     _P_=[0,caml_new_string_b_("Not_found")],
     _aF_=[0,caml_new_string_b_("Assert_failure")],
     _am_=caml_new_string_b_(_a_);
    caml_register_global_ap_(6,_P_);
    caml_register_global_ap_(5,[0,caml_new_string_b_("Division_by_zero")]);
    caml_register_global_ap_(3,_bh_);
    caml_register_global_ap_(2,_ab_);
    var
     _cI_=caml_new_string_b_("output"),
     _cE_=caml_new_string_b_(_eh_),
     _cF_=caml_new_string_b_(_ei_),
     _cK_=caml_new_string_b_("Pervasives.do_at_exit"),
     _cN_=caml_new_string_b_("\\b"),
     _cO_=caml_new_string_b_("\\t"),
     _cP_=caml_new_string_b_("\\n"),
     _cQ_=caml_new_string_b_("\\r"),
     _cM_=caml_new_string_b_("\\\\"),
     _cL_=caml_new_string_b_("\\'"),
     _cT_=caml_new_string_b_(_a_),
     _cS_=caml_new_string_b_("String.blit"),
     _cR_=caml_new_string_b_("String.sub"),
     _cV_=caml_new_string_b_("Buffer.add_substring"),
     _cU_=caml_new_string_b_("Buffer.add: cannot grow buffer"),
     _c$_=caml_new_string_b_(_a_),
     _da_=caml_new_string_b_(_a_),
     _dd_=caml_new_string_b_("%.12g"),
     _de_=caml_new_string_b_(_bX_),
     _df_=caml_new_string_b_(_bX_),
     _db_=caml_new_string_b_(_ao_),
     _dc_=caml_new_string_b_(_ao_),
     _c__=caml_new_string_b_(_cl_),
     _c8_=caml_new_string_b_("neg_infinity"),
     _c9_=caml_new_string_b_("infinity"),
     _c7_=caml_new_string_b_(_bY_),
     _c6_=caml_new_string_b_("printf: bad positional specification (0)."),
     _c5_=caml_new_string_b_("%_"),
     _c4_=[0,caml_new_string_b_("printf.ml"),143,8],
     _c2_=caml_new_string_b_(_ao_),
     _c3_=caml_new_string_b_("Printf: premature end of format string '"),
     _cY_=caml_new_string_b_(_ao_),
     _cZ_=caml_new_string_b_(" in format string '"),
     _c0_=caml_new_string_b_(", at char number "),
     _c1_=caml_new_string_b_("Printf: bad conversion %"),
     _cW_=caml_new_string_b_("Sformat.index_of_int: negative argument "),
     _dg_=caml_new_string_b_(_ci_),
     _dh_=
      [0,
       987910699,
       495797812,
       364182224,
       414272206,
       318284740,
       990407751,
       383018966,
       270373319,
       840823159,
       24560019,
       536292337,
       512266505,
       189156120,
       730249596,
       143776328,
       51606627,
       140166561,
       366354223,
       1003410265,
       700563762,
       981890670,
       913149062,
       526082594,
       1021425055,
       784300257,
       667753350,
       630144451,
       949649812,
       48546892,
       415514493,
       258888527,
       511570777,
       89983870,
       283659902,
       308386020,
       242688715,
       482270760,
       865188196,
       1027664170,
       207196989,
       193777847,
       619708188,
       671350186,
       149669678,
       257044018,
       87658204,
       558145612,
       183450813,
       28133145,
       901332182,
       710253903,
       510646120,
       652377910,
       409934019,
       801085050],
     _dC_=caml_new_string_b_("bad box format"),
     _dD_=caml_new_string_b_("bad box name ho"),
     _dF_=caml_new_string_b_(_bZ_),
     _dE_=caml_new_string_b_(_bZ_),
     _dG_=caml_new_string_b_(_a_),
     _dH_=caml_new_string_b_(_a_),
     _dB_=caml_new_string_b_("bad integer specification"),
     _dA_=caml_new_string_b_("bad format"),
     _dx_=caml_new_string_b_(" (%c)."),
     _dz_=caml_new_string_b_("%c"),
     _dy_=
      caml_new_string_b_
       ("Format.fprintf: %s '%s', giving up at character number %d%s"),
     _dt_=[3,0,3],
     _du_=caml_new_string_b_(_bY_),
     _dp_=caml_new_string_b_(_b0_),
     _dq_=caml_new_string_b_("</"),
     _dm_=caml_new_string_b_(_b0_),
     _dn_=caml_new_string_b_("<"),
     _dk_=caml_new_string_b_("\n"),
     _di_=caml_new_string_b_("Format.Empty_queue"),
     _dj_=[0,caml_new_string_b_(_a_)],
     _dT_=caml_new_string_b_("canvas"),
     _dQ_=caml_new_string_b_(_b1_),
     _dP_=caml_new_string_b_("div"),
     _dR_=caml_new_string_b_("Dom_html.Canvas_not_available"),
     _eg_=caml_new_string_b_("%.1f fps - %.f ms"),
     _d4_=[0,0],
     _d5_=caml_new_string_b_("p0"),
     _d6_=[0,32],
     _d7_=caml_new_string_b_("p1"),
     _d8_=[0,64],
     _d9_=caml_new_string_b_("p2"),
     _d__=[0,96],
     _d$_=caml_new_string_b_("p3"),
     _ea_=[0,1],
     _eb_=caml_new_string_b_(_b2_),
     _ec_=caml_new_string_b_("data1"),
     _ed_=caml_new_string_b_("data2"),
     _d3_=caml_new_string_b_(_b1_),
     _d2_=[0,caml_new_string_b_(_b3_),668,52],
     _ee_=[0,-1,-1,-1,-1],
     _ef_=[_cs_,1,0,0,0,1,0,0,0,1],
     _d1_=[0,caml_new_string_b_(_b3_),533,12],
     _dZ_=caml_new_string_b_("vertices: %d"),
     _d0_=caml_new_string_b_("edges: %d"),
     _dV_=
      caml_new_string_b_
       ("\n  precision highp float;\n  uniform vec2 scale;\n  uniform mat3 transform;\n  attribute vec2 p;\n\n  void main () {\n    gl_Position = vec4((transform * vec3(p, 1)).xy * scale + vec2(-1., -1.), 0., 1.);\n  }\n"),
     _dW_=
      caml_new_string_b_
       ("\n  precision mediump float;\n\n  uniform vec4 color;\n\n  void main () {\n    gl_FragColor = color;\n  }\n"),
     _dX_=
      caml_new_string_b_
       ("\n  precision highp float;\n\n  uniform vec2 scale;\n  uniform mat3 transform;\n  uniform float zoom;\n\n  attribute vec2 p0, p1, p2, p3; // the two extremities of the line\n  attribute lowp vec4 color;  // line color\n/*\n  attribute float w;          // line width\n  attribute float style1, style2; // line style\n  attribute float edge;   // source or target\n  attribute float side;   // which side of the line\n*/\n  attribute vec4 data1, data2;\n\n  varying mediump vec4 dx;\n  varying mediump vec2 dy_hw;\n  varying lowp vec4 col;\n\n  void main () {\n    float w = data1.x;\n    float style1 = data1.y;\n    float style2 = data2.y;\n    float side = data1.z;\n    float edge = data1.w;\n\n    vec2 q1 = (transform * vec3(p1, 1)).xy;\n    vec2 q2 = (transform * vec3(p2, 1)).xy;\n    float hw = w * zoom * (0.5 / 8.);\n\n    vec2 dq = q2 - q1;\n    vec2 t1 = normalize(dq);\n    vec2 n1 = vec2(-t1.y, t1.x);\n\n    float border = hw + 1.;\n\n    float dy = (hw + 1.) * side;\n\n    vec4 d1 = vec4(t1, - dot(t1, q1), 0);\n    vec4 d2 = vec4(0, 0, length(dq), 0) - d1;\n    vec4 d3 = d1;\n    vec4 d4 = d2;\n\n    if (style1 > 0.) {\n      // Line cap\n      q1 = q1 + (border * side) * n1 - border * t1;\n      d1.z = (style1 == 3.) ? (d1.z+hw): d1.z;\n      d3.z = (style1 == 1.) ? (d3.z-hw): d3.z;\n    } else {\n      // Line join\n      vec2 q0 = (transform * vec3(p0, 1)).xy;\n      vec2 t0 = normalize(q1 - q0);\n      vec2 n0 = vec2(-t0.y, t0.x);\n      vec2 n = (n0 + n1) / (1. + dot(n0, n1));\n\nd3 = (style1 == -3.) ? vec4(-normalize(n), dot(normalize(n), q1), 0)  :  vec4(0.);\n\n      q1 = q1 + (border * side) * n;\n      d1 = (style1 == -2.) ? d1 : vec4(0.);\n    }\n\n    if (style2 > 0.) {\n      // Line cap\n      q2 = q2 + (border * side) * n1 + border * t1;\n      d2.z = (style2 == 3.) ? (d2.z+hw): d2.z;\n      d4.z = (style2 == 1.) ? (d4.z-hw): d4.z;\n    } else {\n      // Line join\n      vec2 q3 = (transform * vec3(p3, 1)).xy;\n      vec2 t2 = normalize(q3 - q2);\n      vec2 n2 = vec2(-t2.y, t2.x);\n      vec2 n = (n1 + n2) / (1. + dot(n1, n2));\n  // |n| = sqrt(2/(1. + dot(n1, n2)))\n  // miter if miter_limit^2*(1. + dot(n1, n2)) < 2\n\nd4 = (style2 == -3.) ? vec4(-normalize(n), + dot(normalize(n), q2), 0)  :  vec4(0.);\n\n      q2 = q2 + (border * side) * n;\n      d2 = (style2 == -2.) ? d2 : vec4(0.);\n    }\n\n    vec2 q = edge > 0. ? q1 : q2;\n\n    dy_hw = vec2(dy, hw);\n    col = color;\n\n    dx = vec4(q, 1, 0) * mat4(d1,d2,d3,d4);\n\n    gl_Position = vec4(q * scale + vec2(-1., -1.), 0., 1.);\n  }\n"),
     _dY_=
      caml_new_string_b_
       ("\n  #define FAST 0\n  #define BLUR 0.6\n\n  precision mediump float;\n\n  varying mediump vec4 dx;\n  varying mediump vec2 dy_hw;\n  varying lowp vec4 col;\n\n  void main() {\n    float dy = dy_hw.x; float half_width = dy_hw.y;\n\n    vec2 ds = min (dx.xz, dx.yw);\n\n    float dx2 = min (0., ds.x);\n    float d = sqrt(dx2 * dx2 + dy * dy);\n\n    d = max(d, -ds.y);\n\n    vec3 dists = vec3(half_width - d,- half_width - d, half_width + ds.y);\n\n#if FAST\n    vec3 alphas = clamp(dists * BLUR + 0.5, 0., 1.);\n#else\n    vec3 alphas = smoothstep(0., 1., dists * BLUR + 0.5);\n#endif\n\n    float alpha = /* alphas.z * */ (alphas.x - alphas.y);\n\n    gl_FragColor = col * alpha;\n  }\n");
    function _aD_(_a_){throw [0,_ab_,_a_]}
    function _s_(_a_){throw [0,_bh_,_a_]}
    var _cD_=(1<<31)-1|0;
    function _o_(_a_,_b_)
     {var
       _c_=_a_.getLen(),
       _e_=_b_.getLen(),
       _d_=caml_create_string_x_(_c_+_e_|0);
      caml_blit_string_F_(_a_,0,_d_,0,_c_);
      caml_blit_string_F_(_b_,0,_d_,_c_,_e_);
      return _d_}
    function _ac_(_a_){return caml_new_string_b_(_i_+_a_)}
    var
     _cG_=caml_ml_open_descriptor_out_b4_(1),
     _cH_=caml_ml_open_descriptor_out_b4_(2),
     _aE_=
      [0,
       function(_a_)
        {var _b_=caml_ml_out_channels_list_eN_(0);
         for(;;)
          {if(_b_)
            {var _c_=_b_[2],_d_=_b_[1];
             try {caml_ml_flush_b5_(_d_)}catch(_f_){}
             var _b_=_c_;
             continue}
           return 0}}];
    function _bi_(_a_){return caml_call_gen1_h_(_aE_[1],0)}
    caml_register_named_value_eZ_(_cK_,_bi_);
    function _aG_(_a_)
     {var _c_=0,_b_=_a_;
      for(;;){if(_b_){var _c_=_c_+1|0,_b_=_b_[2];continue}return _c_}}
    function _ad_(_a_)
     {var _b_=_a_,_c_=0;
      for(;;)
       {if(_b_){var _d_=[0,_b_[1],_c_],_b_=_b_[2],_c_=_d_;continue}return _c_}}
    function _f_(_a_,_b_)
     {var _c_=_b_;
      for(;;)
       {if(_c_)
         {var _d_=_c_[2];caml_call_gen1_h_(_a_,_c_[1]);var _c_=_d_;continue}
        return 0}}
    function _bj_(_a_,_b_,_c_)
     {var _e_=_b_,_d_=_c_;
      for(;;)
       {if(_d_)
         {var _f_=_d_[2],_e_=caml_call_gen2_n_(_a_,_e_,_d_[1]),_d_=_f_;
          continue}
        return _e_}}
    function _A_(_a_,_b_)
     {var _c_=caml_create_string_x_(_a_);
      caml_fill_string_em_(_c_,0,_a_,_b_);
      return _c_}
    function _ae_(_a_,_b_,_c_)
     {if(0<=_b_&&0<=_c_&&!((_a_.getLen()-_c_|0)<_b_))
       {var _d_=caml_create_string_x_(_c_);
        caml_blit_string_F_(_a_,_b_,_d_,0,_c_);
        return _d_}
      return _s_(_cR_)}
    function _Q_(_a_,_b_,_c_,_d_,_e_)
     {if
       (0<=
        _e_&&
        0<=
        _b_&&
        !((_a_.getLen()-_e_|0)<_b_)&&
        0<=
        _d_&&
        !((_c_.getLen()-_e_|0)<_d_))
       return caml_blit_string_F_(_a_,_b_,_c_,_d_,_e_);
      return _s_(_cS_)}
    var
     _aH_=caml_sys_const_word_size_e0_(0),
     _R_=caml_mul_a1_(_aH_/8|0,(1<<(_aH_-10|0))-1|0)-1|0;
    function _J_(_a_)
     {var _b_=1<=_a_?_a_:1,_c_=_R_<_b_?_R_:_b_,_d_=caml_create_string_x_(_c_);
      return [0,_d_,0,_c_,_d_]}
    function _af_(_a_){return _ae_(_a_[1],0,_a_[2])}
    function _aI_(_a_,_b_)
     {var _c_=[0,_a_[3]];
      for(;;)
       {if(_c_[1]<(_a_[2]+_b_|0)){_c_[1]=2*_c_[1]|0;continue}
        if(_R_<_c_[1])if((_a_[2]+_b_|0)<=_R_)_c_[1]=_R_;else _aD_(_cU_);
        var _d_=caml_create_string_x_(_c_[1]);
        _Q_(_a_[1],0,_d_,0,_a_[2]);
        _a_[1]=_d_;
        _a_[3]=_c_[1];
        return 0}}
    function _S_(_a_,_b_)
     {var _c_=_a_[2];
      if(_a_[3]<=_c_)_aI_(_a_,1);
      _a_[1].safeSet(_c_,_b_);
      _a_[2]=_c_+1|0;
      return 0}
    function _aJ_(_a_,_b_)
     {var _c_=_b_.getLen(),_d_=_a_[2]+_c_|0;
      if(_a_[3]<_d_)_aI_(_a_,_c_);
      _Q_(_b_,0,_a_[1],_a_[2],_c_);
      _a_[2]=_d_;
      return 0}
    function _u_(_a_){return 0<=_a_?_a_:_aD_(_o_(_cW_,_ac_(_a_)))}
    function _aK_(_a_,_b_){return _u_(_a_+_b_|0)}
    var _cX_=1;
    function _bk_(_a_){return _aK_(_cX_,_a_)}
    function _T_(_a_,_b_,_c_){return _ae_(_a_,_b_,_c_)}
    function _aL_(_a_){return _T_(_a_,0,_a_.getLen())}
    function _bl_(_a_,_b_,_c_)
     {var _d_=_o_(_cZ_,_o_(_a_,_cY_)),_e_=_o_(_c0_,_o_(_ac_(_b_),_d_));
      return _s_(_o_(_c1_,_o_(_A_(1,_c_),_e_)))}
    function _U_(_a_,_b_,_c_){return _bl_(_aL_(_a_),_b_,_c_)}
    function _ag_(_a_){return _s_(_o_(_c3_,_o_(_aL_(_a_),_c2_)))}
    function _D_(_e_,_b_,_c_,_d_)
     {function _h_(_a_)
       {if((_e_.safeGet(_a_)-48|0)<0||9<(_e_.safeGet(_a_)-48|0))return _a_;
        var _b_=_a_+1|0;
        for(;;)
         {var _c_=_e_.safeGet(_b_);
          if(48<=_c_)
           {if(!(58<=_c_)){var _b_=_b_+1|0;continue}var _d_=0}
          else
           if(36===_c_){var _f_=_b_+1|0,_d_=1}else var _d_=0;
          if(!_d_)var _f_=_a_;
          return _f_}}
      var _i_=_h_(_b_+1|0),_f_=_J_((_c_-_i_|0)+10|0);
      _S_(_f_,37);
      var _a_=_i_,_g_=_ad_(_d_);
      for(;;)
       {if(_a_<=_c_)
         {var _j_=_e_.safeGet(_a_);
          if(42===_j_)
           {if(_g_)
             {var _k_=_g_[2];
              _aJ_(_f_,_ac_(_g_[1]));
              var _a_=_h_(_a_+1|0),_g_=_k_;
              continue}
            throw [0,_aF_,_c4_]}
          _S_(_f_,_j_);
          var _a_=_a_+1|0;
          continue}
        return _af_(_f_)}}
    function _bm_(_a_,_b_,_c_,_d_,_e_)
     {var _f_=_D_(_b_,_c_,_d_,_e_);
      if(78!==_a_&&_Z_!==_a_)return _f_;
      _f_.safeSet(_f_.getLen()-1|0,_a7_);
      return _f_}
    function _bn_(_a_)
     {return function(_c_,_b_)
       {var _m_=_c_.getLen();
        function _n_(_a_,_b_)
         {var _o_=40===_a_?41:_a6_;
          function _k_(_a_)
           {var _d_=_a_;
            for(;;)
             {if(_m_<=_d_)return _ag_(_c_);
              if(37===_c_.safeGet(_d_))
               {var _e_=_d_+1|0;
                if(_m_<=_e_)
                 var _f_=_ag_(_c_);
                else
                 {var _g_=_c_.safeGet(_e_),_h_=_g_-40|0;
                  if(_h_<0||1<_h_)
                   {var _l_=_h_-83|0;
                    if(_l_<0||2<_l_)
                     var _j_=1;
                    else
                     switch(_l_)
                      {case 1:var _j_=1;break;
                       case 2:var _i_=1,_j_=0;break;
                       default:var _i_=0,_j_=0}
                    if(_j_){var _f_=_k_(_e_+1|0),_i_=2}}
                  else
                   var _i_=0===_h_?0:1;
                  switch(_i_)
                   {case 1:var _f_=_g_===_o_?_e_+1|0:_U_(_c_,_b_,_g_);break;
                    case 2:break;
                    default:var _f_=_k_(_n_(_g_,_e_+1|0)+1|0)}}
                return _f_}
              var _d_=_d_+1|0;
              continue}}
          return _k_(_b_)}
        return _n_(_a_,_b_)}}
    function _bo_(_i_,_b_,_c_)
     {var _m_=_i_.getLen()-1|0;
      function _s_(_a_)
       {var _l_=_a_;
        a:
        for(;;)
         {if(_l_<_m_)
           {if(37===_i_.safeGet(_l_))
             {var _e_=0,_h_=_l_+1|0;
              for(;;)
               {if(_m_<_h_)
                 var _w_=_ag_(_i_);
                else
                 {var _o_=_i_.safeGet(_h_);
                  if(58<=_o_)
                   {if(95===_o_){var _e_=1,_h_=_h_+1|0;continue}}
                  else
                   if(32<=_o_)
                    switch(_o_-32|0)
                     {case 1:
                      case 2:
                      case 4:
                      case 5:
                      case 6:
                      case 7:
                      case 8:
                      case 9:
                      case 12:
                      case 15:break;
                      case 0:
                      case 3:
                      case 11:
                      case 13:var _h_=_h_+1|0;continue;
                      case 10:var _h_=caml_call_gen3_j_(_b_,_e_,_h_,_I_);continue;
                      default:var _h_=_h_+1|0;continue}
                  var _d_=_h_;
                  b:
                  for(;;)
                   {if(_m_<_d_)
                     var _f_=_ag_(_i_);
                    else
                     {var _k_=_i_.safeGet(_d_);
                      if(126<=_k_)
                       var _g_=0;
                      else
                       switch(_k_)
                        {case 78:
                         case 88:
                         case _ay_:
                         case _I_:
                         case _au_:
                         case _a7_:
                         case _a8_:
                          var _f_=caml_call_gen3_j_(_b_,_e_,_d_,_I_),_g_=1;break;
                         case 69:
                         case 70:
                         case 71:
                         case _cp_:
                         case _ba_:
                         case _a$_:
                          var _f_=caml_call_gen3_j_(_b_,_e_,_d_,_ba_),_g_=1;break;
                         case 33:
                         case 37:
                         case 44:
                         case 64:var _f_=_d_+1|0,_g_=1;break;
                         case 83:
                         case 91:
                         case ___:
                          var _f_=caml_call_gen3_j_(_b_,_e_,_d_,___),_g_=1;break;
                         case 97:
                         case _at_:
                         case _a4_:
                          var _f_=caml_call_gen3_j_(_b_,_e_,_d_,_k_),_g_=1;break;
                         case 76:
                         case _bd_:
                         case _Z_:
                          var _t_=_d_+1|0;
                          if(_m_<_t_)
                           {var _f_=caml_call_gen3_j_(_b_,_e_,_d_,_I_),_g_=1}
                          else
                           {var _q_=_i_.safeGet(_t_)-88|0;
                            if(_q_<0||32<_q_)
                             var _r_=1;
                            else
                             switch(_q_)
                              {case 0:
                               case 12:
                               case 17:
                               case 23:
                               case 29:
                               case 32:
                                var
                                 _f_=
                                  caml_call_gen2_n_
                                   (_c_,caml_call_gen3_j_(_b_,_e_,_d_,_k_),_I_),
                                 _g_=1,
                                 _r_=0;
                                break;
                               default:var _r_=1}
                            if(_r_){var _f_=caml_call_gen3_j_(_b_,_e_,_d_,_I_),_g_=1}}
                          break;
                         case 67:
                         case 99:
                          var _f_=caml_call_gen3_j_(_b_,_e_,_d_,99),_g_=1;break;
                         case 66:
                         case 98:
                          var _f_=caml_call_gen3_j_(_b_,_e_,_d_,66),_g_=1;break;
                         case 41:
                         case _a6_:
                          var _f_=caml_call_gen3_j_(_b_,_e_,_d_,_k_),_g_=1;break;
                         case 40:
                          var _f_=_s_(caml_call_gen3_j_(_b_,_e_,_d_,_k_)),_g_=1;break;
                         case _as_:
                          var
                           _u_=caml_call_gen3_j_(_b_,_e_,_d_,_k_),
                           _v_=caml_call_gen2_n_(_bn_(_k_),_i_,_u_),
                           _p_=_u_;
                          for(;;)
                           {if(_p_<(_v_-2|0))
                             {var _p_=caml_call_gen2_n_(_c_,_p_,_i_.safeGet(_p_));
                              continue}
                            var _d_=_v_-1|0;
                            continue b}
                         default:var _g_=0}
                      if(!_g_)var _f_=_U_(_i_,_d_,_k_)}
                    var _w_=_f_;
                    break}}
                var _l_=_w_;
                continue a}}
            var _l_=_l_+1|0;
            continue}
          return _l_}}
      _s_(0);
      return 0}
    function _aM_(_a_)
     {var _d_=[0,0,0,0];
      function _b_(_a_,_b_,_c_)
       {var _f_=41!==_c_?1:0,_g_=_f_?_a6_!==_c_?1:0:_f_;
        if(_g_)
         {var _e_=97===_c_?2:1;
          if(_at_===_c_)_d_[3]=_d_[3]+1|0;
          if(_a_)_d_[2]=_d_[2]+_e_|0;else _d_[1]=_d_[1]+_e_|0}
        return _b_+1|0}
      _bo_(_a_,_b_,function(_a_,_b_){return _a_+1|0});
      return _d_[1]}
    function _bp_(_i_,_h_)
     {var _c_=_aM_(_h_);
      if(_c_<0||6<_c_)
       {var
         _j_=
          function(_l_,_b_)
           {if(_c_<=_l_)
             {var
               _m_=caml_make_vect_k_(_c_,0),
               _o_=
                function(_a_,_b_)
                 {return caml_array_set_g_(_m_,(_c_-_a_|0)-1|0,_b_)},
               _d_=0,
               _a_=_b_;
              for(;;)
               {if(_a_)
                 {var _e_=_a_[2],_f_=_a_[1];
                  if(_e_){_o_(_d_,_f_);var _d_=_d_+1|0,_a_=_e_;continue}
                  _o_(_d_,_f_)}
                return caml_call_gen2_n_(_i_,_h_,_m_)}}
            return function(_a_){return _j_(_l_+1|0,[0,_a_,_b_])}};
        return _j_(0,0)}
      switch(_c_)
       {case 1:
         return function(_a_)
          {var _b_=caml_make_vect_k_(1,0);
           caml_array_set_g_(_b_,0,_a_);
           return caml_call_gen2_n_(_i_,_h_,_b_)};
        case 2:
         return function(_a_,_b_)
          {var _c_=caml_make_vect_k_(2,0);
           caml_array_set_g_(_c_,0,_a_);
           caml_array_set_g_(_c_,1,_b_);
           return caml_call_gen2_n_(_i_,_h_,_c_)};
        case 3:
         return function(_a_,_b_,_c_)
          {var _d_=caml_make_vect_k_(3,0);
           caml_array_set_g_(_d_,0,_a_);
           caml_array_set_g_(_d_,1,_b_);
           caml_array_set_g_(_d_,2,_c_);
           return caml_call_gen2_n_(_i_,_h_,_d_)};
        case 4:
         return function(_a_,_b_,_c_,_d_)
          {var _e_=caml_make_vect_k_(4,0);
           caml_array_set_g_(_e_,0,_a_);
           caml_array_set_g_(_e_,1,_b_);
           caml_array_set_g_(_e_,2,_c_);
           caml_array_set_g_(_e_,3,_d_);
           return caml_call_gen2_n_(_i_,_h_,_e_)};
        case 5:
         return function(_a_,_b_,_c_,_d_,_e_)
          {var _f_=caml_make_vect_k_(5,0);
           caml_array_set_g_(_f_,0,_a_);
           caml_array_set_g_(_f_,1,_b_);
           caml_array_set_g_(_f_,2,_c_);
           caml_array_set_g_(_f_,3,_d_);
           caml_array_set_g_(_f_,4,_e_);
           return caml_call_gen2_n_(_i_,_h_,_f_)};
        case 6:
         return function(_a_,_b_,_c_,_d_,_e_,_f_)
          {var _j_=caml_make_vect_k_(6,0);
           caml_array_set_g_(_j_,0,_a_);
           caml_array_set_g_(_j_,1,_b_);
           caml_array_set_g_(_j_,2,_c_);
           caml_array_set_g_(_j_,3,_d_);
           caml_array_set_g_(_j_,4,_e_);
           caml_array_set_g_(_j_,5,_f_);
           return caml_call_gen2_n_(_i_,_h_,_j_)};
        default:return caml_call_gen2_n_(_i_,_h_,[0])}}
    function _bq_(_a_,_b_,_c_)
     {var _h_=_a_.safeGet(_c_);
      if((_h_-48|0)<0||9<(_h_-48|0))return caml_call_gen2_n_(_b_,0,_c_);
      var _e_=_h_-48|0,_d_=_c_+1|0;
      for(;;)
       {var _f_=_a_.safeGet(_d_);
        if(48<=_f_)
         {if(!(58<=_f_)){var _e_=(10*_e_|0)+(_f_-48|0)|0,_d_=_d_+1|0;continue}
          var _g_=0}
        else
         if(36===_f_)
          if(0===_e_)
           {var _i_=_aD_(_c6_),_g_=1}
          else
           {var _i_=caml_call_gen2_n_(_b_,[0,_u_(_e_-1|0)],_d_+1|0),_g_=1}
         else
          var _g_=0;
        if(!_g_)var _i_=caml_call_gen2_n_(_b_,0,_c_);
        return _i_}}
    function _t_(_a_,_b_){return _a_?_b_:_bk_(_b_)}
    function _br_(_a_,_b_){return _a_?_a_[1]:_b_}
    function _ah_(_a_,_k_,_c_,_v_,_e_,_f_,_g_,_h_,_i_)
     {function _s_(_a_,_b_){return caml_array_get_d_(_k_,_br_(_a_,_b_))}
      function _aG_(_m_,_k_,_c_,_d_)
       {var _b_=_d_;
        for(;;)
         {var _ak_=_a_.safeGet(_b_)-32|0;
          if(!(_ak_<0||25<_ak_))
           switch(_ak_)
            {case 1:
             case 2:
             case 4:
             case 5:
             case 6:
             case 7:
             case 8:
             case 9:
             case 12:
             case 15:break;
             case 10:
              return _bq_
                      (_a_,
                       function(_a_,_b_)
                        {var _d_=[0,_s_(_a_,_k_),_c_];
                         return _aG_(_m_,_t_(_a_,_k_),_d_,_b_)},
                       _b_+1|0);
             default:var _b_=_b_+1|0;continue}
          var _u_=_a_.safeGet(_b_);
          if(124<=_u_)
           var _p_=0;
          else
           switch(_u_)
            {case 78:
             case 88:
             case _ay_:
             case _I_:
             case _au_:
             case _a7_:
             case _a8_:
              var
               _be_=_s_(_m_,_k_),
               _bf_=caml_format_int_aq_(_bm_(_u_,_a_,_v_,_b_,_c_),_be_),
               _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_bf_,_b_+1|0),
               _p_=1;
              break;
             case 69:
             case 71:
             case _cp_:
             case _ba_:
             case _a$_:
              var
               _a3_=_s_(_m_,_k_),
               _a5_=caml_format_float_b6_(_D_(_a_,_v_,_b_,_c_),_a3_),
               _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_a5_,_b_+1|0),
               _p_=1;
              break;
             case 76:
             case _bd_:
             case _Z_:
              var _ao_=_a_.safeGet(_b_+1|0)-88|0;
              if(_ao_<0||32<_ao_)
               var _ar_=1;
              else
               switch(_ao_)
                {case 0:
                 case 12:
                 case 17:
                 case 23:
                 case 29:
                 case 32:
                  var _$_=_b_+1|0,_ap_=_u_-_bd_|0;
                  if(_ap_<0||2<_ap_)
                   var _av_=0;
                  else
                   {switch(_ap_)
                     {case 1:var _av_=0,_aw_=0;break;
                      case 2:
                       var
                        _bc_=_s_(_m_,_k_),
                        _aM_=caml_format_int_aq_(_D_(_a_,_v_,_$_,_c_),_bc_),
                        _aw_=1;
                       break;
                      default:
                       var
                        _bb_=_s_(_m_,_k_),
                        _aM_=caml_format_int_aq_(_D_(_a_,_v_,_$_,_c_),_bb_),
                        _aw_=1}
                    if(_aw_){var _aL_=_aM_,_av_=1}}
                  if(!_av_)
                   {var
                     _a__=_s_(_m_,_k_),
                     _aL_=caml_int64_format_es_(_D_(_a_,_v_,_$_,_c_),_a__)}
                  var
                   _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_aL_,_$_+1|0),
                   _p_=1,
                   _ar_=0;
                  break;
                 default:var _ar_=1}
              if(_ar_)
               {var
                 _a6_=_s_(_m_,_k_),
                 _a9_=caml_format_int_aq_(_bm_(_Z_,_a_,_v_,_b_,_c_),_a6_),
                 _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_a9_,_b_+1|0),
                 _p_=1}
              break;
             case 37:
             case 64:
              var _q_=caml_call_gen3_j_(_e_,_k_,_A_(1,_u_),_b_+1|0),_p_=1;
              break;
             case 83:
             case ___:
              var _E_=_s_(_m_,_k_);
              if(___===_u_)
               var _F_=_E_;
              else
               {var _l_=[0,0],_aA_=_E_.getLen()-1|0,_aR_=0;
                if(!(_aA_<0))
                 {var _R_=_aR_;
                  for(;;)
                   {var
                     _C_=_E_.safeGet(_R_),
                     _bj_=
                      14<=_C_
                       ?34===_C_?1:92===_C_?1:0
                       :11<=_C_?13<=_C_?1:0:8<=_C_?1:0,
                     _aU_=_bj_?2:caml_is_printable_a2_(_C_)?1:4;
                    _l_[1]=_l_[1]+_aU_|0;
                    var _aV_=_R_+1|0;
                    if(_aA_!==_R_){var _R_=_aV_;continue}
                    break}}
                if(_l_[1]===_E_.getLen())
                 var _aO_=_E_;
                else
                 {var _r_=caml_create_string_x_(_l_[1]);
                  _l_[1]=0;
                  var _aB_=_E_.getLen()-1|0,_aS_=0;
                  if(!(_aB_<0))
                   {var _P_=_aS_;
                    for(;;)
                     {var _B_=_E_.safeGet(_P_),_G_=_B_-34|0;
                      if(_G_<0||58<_G_)
                       if(-20<=_G_)
                        var _aa_=1;
                       else
                        {switch(_G_+34|0)
                          {case 8:
                            _r_.safeSet(_l_[1],92);
                            _l_[1]++;
                            _r_.safeSet(_l_[1],98);
                            var _O_=1;
                            break;
                           case 9:
                            _r_.safeSet(_l_[1],92);
                            _l_[1]++;
                            _r_.safeSet(_l_[1],_a4_);
                            var _O_=1;
                            break;
                           case 10:
                            _r_.safeSet(_l_[1],92);
                            _l_[1]++;
                            _r_.safeSet(_l_[1],_Z_);
                            var _O_=1;
                            break;
                           case 13:
                            _r_.safeSet(_l_[1],92);
                            _l_[1]++;
                            _r_.safeSet(_l_[1],_at_);
                            var _O_=1;
                            break;
                           default:var _aa_=1,_O_=0}
                         if(_O_)var _aa_=0}
                      else
                       var
                        _aa_=
                         (_G_-1|0)<0||56<(_G_-1|0)
                          ?(_r_.safeSet(_l_[1],92),_l_[1]++,_r_.safeSet(_l_[1],_B_),0)
                          :1;
                      if(_aa_)
                       if(caml_is_printable_a2_(_B_))
                        _r_.safeSet(_l_[1],_B_);
                       else
                        {_r_.safeSet(_l_[1],92);
                         _l_[1]++;
                         _r_.safeSet(_l_[1],48+(_B_/_ay_|0)|0);
                         _l_[1]++;
                         _r_.safeSet(_l_[1],48+((_B_/10|0)%10|0)|0);
                         _l_[1]++;
                         _r_.safeSet(_l_[1],48+(_B_%10|0)|0)}
                      _l_[1]++;
                      var _aT_=_P_+1|0;
                      if(_aB_!==_P_){var _P_=_aT_;continue}
                      break}}
                  var _aO_=_r_}
                var _F_=_o_(_df_,_o_(_aO_,_de_))}
              if(_b_===(_v_+1|0))
               var _aN_=_F_;
              else
               {var _N_=_D_(_a_,_v_,_b_,_c_);
                try
                 {var _ac_=0,_y_=1;
                  for(;;)
                   {if(_N_.getLen()<=_y_)
                     var _aC_=[0,0,_ac_];
                    else
                     {var _ad_=_N_.safeGet(_y_);
                      if(49<=_ad_)
                       if(58<=_ad_)
                        var _ax_=0;
                       else
                        {var
                          _aC_=
                           [0,
                            caml_int_of_string_b7_
                             (_ae_(_N_,_y_,(_N_.getLen()-_y_|0)-1|0)),
                            _ac_],
                          _ax_=1}
                      else
                       {if(45===_ad_){var _ac_=1,_y_=_y_+1|0;continue}var _ax_=0}
                      if(!_ax_){var _y_=_y_+1|0;continue}}
                    var _ah_=_aC_;
                    break}}
                catch(_f_)
                 {if(_f_[1]!==_ab_)throw _f_;var _ah_=_bl_(_N_,0,___)}
                var _T_=_ah_[1],_H_=_F_.getLen(),_aW_=_ah_[2],_V_=0,_aX_=32;
                if(_T_===_H_&&0===_V_){var _ai_=_F_,_aQ_=1}else var _aQ_=0;
                if(!_aQ_)
                 if(_T_<=_H_)
                  var _ai_=_ae_(_F_,_V_,_H_);
                 else
                  {var _ag_=_A_(_T_,_aX_);
                   if(_aW_)
                    _Q_(_F_,_V_,_ag_,0,_H_);
                   else
                    _Q_(_F_,_V_,_ag_,_T_-_H_|0,_H_);
                   var _ai_=_ag_}
                var _aN_=_ai_}
              var _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_aN_,_b_+1|0),_p_=1;
              break;
             case 67:
             case 99:
              var _w_=_s_(_m_,_k_);
              if(99===_u_)
               var _aI_=_A_(1,_w_);
              else
               {if(39===_w_)
                 var _z_=_cL_;
                else
                 if(92===_w_)
                  var _z_=_cM_;
                 else
                  {if(14<=_w_)
                    var _K_=0;
                   else
                    switch(_w_)
                     {case 8:var _z_=_cN_,_K_=1;break;
                      case 9:var _z_=_cO_,_K_=1;break;
                      case 10:var _z_=_cP_,_K_=1;break;
                      case 13:var _z_=_cQ_,_K_=1;break;
                      default:var _K_=0}
                   if(!_K_)
                    if(caml_is_printable_a2_(_w_))
                     {var _az_=caml_create_string_x_(1);
                      _az_.safeSet(0,_w_);
                      var _z_=_az_}
                    else
                     {var _L_=caml_create_string_x_(4);
                      _L_.safeSet(0,92);
                      _L_.safeSet(1,48+(_w_/_ay_|0)|0);
                      _L_.safeSet(2,48+((_w_/10|0)%10|0)|0);
                      _L_.safeSet(3,48+(_w_%10|0)|0);
                      var _z_=_L_}}
                var _aI_=_o_(_dc_,_o_(_z_,_db_))}
              var _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_aI_,_b_+1|0),_p_=1;
              break;
             case 66:
             case 98:
              var
               _a0_=_b_+1|0,
               _a1_=_s_(_m_,_k_)?_cE_:_cF_,
               _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_a1_,_a0_),
               _p_=1;
              break;
             case 40:
             case _as_:
              var
               _al_=_s_(_m_,_k_),
               _aH_=caml_call_gen2_n_(_bn_(_u_),_a_,_b_+1|0);
              if(_as_===_u_)
               {var
                 _W_=_J_(_al_.getLen()),
                 _aD_=function(_a_,_b_){_S_(_W_,_b_);return _a_+1|0};
                _bo_
                 (_al_,
                  function(_a_,_b_,_c_)
                   {if(_a_)_aJ_(_W_,_c5_);else _S_(_W_,37);
                    return _aD_(_b_,_c_)},
                  _aD_);
                var
                 _aY_=_af_(_W_),
                 _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_aY_,_aH_),
                 _p_=1}
              else
               {var _q_=caml_call_gen3_j_(_i_,_t_(_m_,_k_),_al_,_aH_),_p_=1}
              break;
             case 33:var _q_=caml_call_gen2_n_(_h_,_k_,_b_+1|0),_p_=1;break;
             case 41:
              var _q_=caml_call_gen3_j_(_e_,_k_,_c$_,_b_+1|0),_p_=1;break;
             case 44:
              var _q_=caml_call_gen3_j_(_e_,_k_,_da_,_b_+1|0),_p_=1;break;
             case 70:
              var _am_=_s_(_m_,_k_);
              if(0===_c_)
               var _aK_=_dd_;
              else
               {var _aj_=_D_(_a_,_v_,_b_,_c_);
                if(70===_u_)_aj_.safeSet(_aj_.getLen()-1|0,_a$_);
                var _aK_=_aj_}
              var _aF_=caml_classify_float_el_(_am_);
              if(3===_aF_)
               var _an_=_am_<0?_c8_:_c9_;
              else
               if(4<=_aF_)
                var _an_=_c__;
               else
                {var
                  _Y_=caml_format_float_b6_(_aK_,_am_),
                  _X_=0,
                  _aZ_=_Y_.getLen();
                 for(;;)
                  {if(_aZ_<=_X_)
                    var _aE_=_o_(_Y_,_c7_);
                   else
                    {var
                      _M_=_Y_.safeGet(_X_)-46|0,
                      _bp_=
                       _M_<0||23<_M_?55===_M_?1:0:(_M_-1|0)<0||21<(_M_-1|0)?1:0;
                     if(!_bp_){var _X_=_X_+1|0;continue}
                     var _aE_=_Y_}
                   var _an_=_aE_;
                   break}}
              var _q_=caml_call_gen3_j_(_e_,_t_(_m_,_k_),_an_,_b_+1|0),_p_=1;
              break;
             case 91:var _q_=_U_(_a_,_b_,_u_),_p_=1;break;
             case 97:
              var
               _bg_=_s_(_m_,_k_),
               _aP_=_bk_(_br_(_m_,_k_)),
               _bh_=_s_(0,_aP_),
               _q_=caml_call_gen4_b8_(_f_,_t_(_m_,_aP_),_bg_,_bh_,_b_+1|0),
               _p_=1;
              break;
             case _at_:var _q_=_U_(_a_,_b_,_u_),_p_=1;break;
             case _a4_:
              var
               _bi_=_s_(_m_,_k_),
               _q_=caml_call_gen3_j_(_g_,_t_(_m_,_k_),_bi_,_b_+1|0),
               _p_=1;
              break;
             default:var _p_=0}
          if(!_p_)var _q_=_U_(_a_,_b_,_u_);
          return _q_}}
      var _b_=_v_+1|0,_l_=0;
      return _bq_(_a_,function(_a_,_b_){return _aG_(_a_,_c_,_l_,_b_)},_b_)}
    function _aN_(_a_)
     {function _d_(_a_){var _b_=_af_(_a_);_a_[2]=0;return _b_}
      var _f_=_J_(2*_a_.getLen()|0),_j_=1;
      function _g_(_a_){return _aJ_(_f_,_a_)}
      function _i_(_k_,_b_,_c_,_d_)
       {var _l_=_c_.getLen();
        function _e_(_a_,_b_)
         {var _e_=_b_;
          for(;;)
           {if(_l_<=_e_)return caml_call_gen1_h_(_k_,_f_);
            var _g_=_c_.safeGet(_e_);
            if(37===_g_)return _ah_(_c_,_d_,_a_,_e_,_m_,_o_,_p_,_q_,_r_);
            _S_(_f_,_g_);
            var _e_=_e_+1|0;
            continue}}
        function _m_(_a_,_b_,_c_){_g_(_b_);return _e_(_a_,_c_)}
        function _o_(_a_,_b_,_c_,_d_)
         {if(_j_)
           _g_(caml_call_gen2_n_(_b_,0,_c_));
          else
           caml_call_gen2_n_(_b_,_f_,_c_);
          return _e_(_a_,_d_)}
        function _p_(_a_,_b_,_c_)
         {if(_j_)
           _g_(caml_call_gen1_h_(_b_,0));
          else
           caml_call_gen1_h_(_b_,_f_);
          return _e_(_a_,_c_)}
        function _q_(_a_,_b_){return _e_(_a_,_b_)}
        function _r_(_a_,_b_,_c_)
         {var _f_=_aK_(_aM_(_b_),_a_);
          return _i_(function(_a_){return _e_(_f_,_c_)},_a_,_b_,_d_)}
        return _e_(_b_,0)}
      var _c_=_u_(0);
      return _bp_(function(_a_,_b_){return _i_(_d_,_c_,_a_,_b_)},_a_)}
    var _bs_=[0,0];
    32===_aH_;
    var _ai_=[0,_dh_.slice(),0];
    function _bt_(_a_,_b_)
     {var _c_=[0,[0,_a_,0]],_d_=_b_[1];
      if(_d_){var _e_=_d_[1];_b_[1]=_c_;_e_[2]=_c_;return 0}
      _b_[1]=_c_;
      _b_[2]=_c_;
      return 0}
    var _aO_=[0,_di_];
    function _bu_(_a_)
     {var _b_=_a_[2];
      if(_b_)
       {var _c_=_b_[1],_d_=_c_[2],_e_=_c_[1];
        _a_[2]=_d_;
        if(0===_d_)_a_[1]=0;
        return _e_}
      throw [0,_aO_]}
    function _V_(_a_,_b_){_a_[13]=_a_[13]+_b_[3]|0;return _bt_(_b_,_a_[27])}
    var _bv_=1000000010;
    function _aP_(_a_,_b_)
     {return caml_call_gen3_j_(_a_[17],_b_,0,_b_.getLen())}
    function _aj_(_a_){return caml_call_gen1_h_(_a_[19],0)}
    function _bw_(_a_,_b_){return caml_call_gen1_h_(_a_[20],_b_)}
    function _z_(_a_,_b_,_c_)
     {_aj_(_a_);
      _a_[11]=1;
      var
       _d_=(_a_[6]-_c_|0)+_b_|0,
       _e_=_a_[8],
       _f_=caml_lessequal_eI_(_e_,_d_)?_e_:_d_;
      _a_[10]=_f_;
      _a_[9]=_a_[6]-_a_[10]|0;
      return _bw_(_a_,_a_[10])}
    function _bx_(_a_,_b_){return _z_(_a_,0,_b_)}
    function _K_(_a_,_b_){_a_[9]=_a_[9]-_b_|0;return _bw_(_a_,_b_)}
    function _by_(_a_)
     {try
       {for(;;)
         {var _n_=_a_[27][2];
          if(!_n_)throw [0,_aO_];
          var
           _i_=_n_[1][1],
           _m_=_i_[1],
           _b_=_i_[2],
           _L_=_m_<0?1:0,
           _Y_=_i_[3],
           _Z_=_L_?(_a_[13]-_a_[12]|0)<_a_[9]?1:0:_L_,
           _M_=1-_Z_;
          if(_M_)
           {_bu_(_a_[27]);
            var _g_=0<=_m_?_m_:_bv_;
            if(typeof _b_===_ck_)
             switch(_b_)
              {case 1:var _w_=_a_[2];if(_w_)_a_[2]=_w_[2];break;
               case 2:var _x_=_a_[3];if(_x_)_a_[3]=_x_[2];break;
               case 3:
                var _y_=_a_[2];
                if(_y_)_bx_(_a_,_y_[1][2]);else _aj_(_a_);
                break;
               case 4:
                if(_a_[10]!==(_a_[6]-_a_[9]|0))
                 {var _s_=_bu_(_a_[27]),_N_=_s_[1];
                  _a_[12]=_a_[12]-_s_[3]|0;
                  _a_[9]=_a_[9]+_N_|0}
                break;
               case 5:
                var _j_=_a_[5];
                if(_j_)
                 {var _O_=_j_[2];
                  _aP_(_a_,caml_call_gen1_h_(_a_[24],_j_[1]));
                  _a_[5]=_O_}
                break;
               default:
                var _t_=_a_[3];
                if(_t_)
                 {var
                   _u_=_t_[1][1],
                   _v_=
                    function(_a_,_b_)
                     {if(_b_)
                       {var _c_=_b_[1],_d_=_b_[2];
                        return caml_lessthan_a3_(_a_,_c_)
                                ?[0,_a_,_b_]
                                :[0,_c_,_v_(_a_,_d_)]}
                      return [0,_a_,0]};
                  _u_[1]=_v_(_a_[6]-_a_[9]|0,_u_[1])}}
            else
             switch(_b_[0])
              {case 1:
                var _c_=_b_[2],_e_=_b_[1],_A_=_a_[2];
                if(_A_)
                 {var _B_=_A_[1],_d_=_B_[2];
                  switch(_B_[1])
                   {case 1:_z_(_a_,_c_,_d_);break;
                    case 2:_z_(_a_,_c_,_d_);break;
                    case 3:
                     if(_a_[9]<_g_)_z_(_a_,_c_,_d_);else _K_(_a_,_e_);break;
                    case 4:
                     if(_a_[11])
                      _K_(_a_,_e_);
                     else
                      if(_a_[9]<_g_)
                       _z_(_a_,_c_,_d_);
                      else
                       if(((_a_[6]-_d_|0)+_c_|0)<_a_[10])
                        _z_(_a_,_c_,_d_);
                       else
                        _K_(_a_,_e_);
                     break;
                    case 5:_K_(_a_,_e_);break;
                    default:_K_(_a_,_e_)}}
                break;
               case 2:
                var _k_=_a_[6]-_a_[9]|0,_C_=_a_[3],_R_=_b_[2],_S_=_b_[1];
                if(_C_)
                 {var _D_=_C_[1][1],_F_=_D_[1];
                  if(_F_)
                   {var _U_=_F_[1];
                    try
                     {var _f_=_D_[1];
                      for(;;)
                       {if(!_f_)throw [0,_P_];
                        var _E_=_f_[1],_T_=_f_[2];
                        if(!caml_greaterequal_b9_(_E_,_k_)){var _f_=_T_;continue}
                        var _G_=_E_;
                        break}}
                    catch(_f_){if(_f_[1]!==_P_)throw _f_;var _G_=_U_}
                    var _l_=_G_}
                  else
                   var _l_=_k_;
                  var _H_=_l_-_k_|0;
                  if(0<=_H_)_K_(_a_,_H_+_S_|0);else _z_(_a_,_l_+_R_|0,_a_[6])}
                break;
               case 3:
                var _I_=_b_[2],_V_=_b_[1];
                if(_a_[8]<(_a_[6]-_a_[9]|0))
                 {var _o_=_a_[2];
                  if(_o_)
                   {var
                     _p_=_o_[1],
                     _q_=_p_[2],
                     _r_=_p_[1],
                     ___=_a_[9]<_q_?0===_r_?0:5<=_r_?1:(_bx_(_a_,_q_),1):0}
                  else
                   _aj_(_a_)}
                var _W_=_a_[9]-_V_|0,_X_=1===_I_?1:_a_[9]<_g_?_I_:5;
                _a_[2]=[0,[0,_X_,_W_],_a_[2]];
                break;
               case 4:_a_[3]=[0,_b_[1],_a_[3]];break;
               case 5:
                var _J_=_b_[1];
                _aP_(_a_,caml_call_gen1_h_(_a_[23],_J_));
                _a_[5]=[0,_J_,_a_[5]];
                break;
               default:
                var _Q_=_b_[1];_a_[9]=_a_[9]-_g_|0;_aP_(_a_,_Q_);_a_[11]=0}
            _a_[12]=_Y_+_a_[12]|0;
            continue}
          break}}
      catch(_f_){if(_f_[1]===_aO_)return 0;throw _f_}
      return _M_}
    function _bz_(_a_,_b_){_V_(_a_,_b_);return _by_(_a_)}
    function _L_(_a_,_b_,_c_){return [0,_a_,_b_,_c_]}
    function _bA_(_a_,_b_,_c_){return _bz_(_a_,_L_(_b_,[0,_c_],_b_))}
    var _bB_=[0,[0,-1,_L_(-1,_dj_,0)],0];
    function _bC_(_a_){_a_[1]=_bB_;return 0}
    function _aQ_(_a_,_b_)
     {var _d_=_a_[1];
      if(_d_)
       {var _e_=_d_[1],_c_=_e_[2],_f_=_c_[1],_g_=_d_[2],_h_=_c_[2];
        if(_e_[1]<_a_[12])return _bC_(_a_);
        if(typeof _h_!==_ck_)
         switch(_h_[0])
          {case 1:
           case 2:
            var _j_=_b_?(_c_[1]=_a_[13]+_f_|0,_a_[1]=_g_,0):_b_;return _j_;
           case 3:
            var _i_=1-_b_,_k_=_i_?(_c_[1]=_a_[13]+_f_|0,_a_[1]=_g_,0):_i_;
            return _k_;
           default:}
        return 0}
      return 0}
    function _bD_(_a_,_b_,_c_)
     {_V_(_a_,_c_);
      if(_b_)_aQ_(_a_,1);
      _a_[1]=[0,[0,_a_[13],_c_],_a_[1]];
      return 0}
    function _ak_(_a_,_b_,_c_)
     {_a_[14]=_a_[14]+1|0;
      if(_a_[14]<_a_[15])return _bD_(_a_,0,_L_(-_a_[13]|0,[3,_b_,_c_],0));
      var _d_=_a_[14]===_a_[15]?1:0;
      if(_d_){var _e_=_a_[16];return _bA_(_a_,_e_.getLen(),_e_)}
      return _d_}
    function _bE_(_a_,_b_)
     {var _c_=1<_a_[14]?1:0;
      if(_c_)
       {if(_a_[14]<_a_[15]){_V_(_a_,[0,0,1,0]);_aQ_(_a_,1);_aQ_(_a_,0)}
        _a_[14]=_a_[14]-1|0;
        var _d_=0}
      else
       var _d_=_c_;
      return _d_}
    function _aR_(_a_,_b_)
     {if(_a_[21]){_a_[4]=[0,_b_,_a_[4]];caml_call_gen1_h_(_a_[25],_b_)}
      var _c_=_a_[22];
      return _c_?_V_(_a_,[0,0,[5,_b_],0]):_c_}
    function _aS_(_a_,_b_)
     {for(;;)
       {if(1<_a_[14]){_bE_(_a_,0);continue}
        _a_[13]=_bv_;
        _by_(_a_);
        if(_b_)_aj_(_a_);
        _a_[12]=1;
        _a_[13]=1;
        var _c_=_a_[27];
        _c_[1]=0;
        _c_[2]=0;
        _bC_(_a_);
        _a_[2]=0;
        _a_[3]=0;
        _a_[4]=0;
        _a_[5]=0;
        _a_[10]=0;
        _a_[14]=0;
        _a_[9]=_a_[6];
        return _ak_(_a_,0,3)}}
    function _aT_(_a_,_b_,_c_)
     {var _d_=_a_[14]<_a_[15]?1:0;return _d_?_bA_(_a_,_b_,_c_):_d_}
    function _bF_(_a_,_b_,_c_){return _aT_(_a_,_b_,_c_)}
    function _aU_(_a_,_b_){_aS_(_a_,0);return caml_call_gen1_h_(_a_[18],0)}
    function _aV_(_a_,_b_,_c_)
     {var _d_=_a_[14]<_a_[15]?1:0;
      return _d_?_bD_(_a_,1,_L_(-_a_[13]|0,[1,_b_,_c_],_b_)):_d_}
    function _aW_(_a_,_b_){return _aV_(_a_,1,0)}
    var _bG_=_A_(80,32);
    function _dl_(_a_){return _o_(_dn_,_o_(_a_,_dm_))}
    function _do_(_a_){return _o_(_dq_,_o_(_a_,_dp_))}
    function _dr_(_a_){return 0}
    function _ds_(_a_){return 0}
    function _bH_(_a_,_b_)
     {function _f_(_a_){return 0}
      var _d_=[0,0,0];
      function _g_(_a_){return 0}
      var _e_=_L_(-1,_dt_,0);
      _bt_(_e_,_d_);
      var
       _c_=
        [0,
         [0,[0,1,_e_],_bB_],
         0,
         0,
         0,
         0,
         78,
         10,
         78-10|0,
         78,
         0,
         1,
         1,
         1,
         1,
         _cD_,
         _du_,
         _a_,
         _b_,
         _g_,
         _f_,
         0,
         0,
         _dl_,
         _do_,
         _dr_,
         _ds_,
         _d_];
      _c_[19]=function(_a_){return caml_call_gen3_j_(_c_[17],_dk_,0,1)};
      _c_[20]=
      function(_a_)
       {var _b_=_a_;
        for(;;)
         {var _d_=0<_b_?1:0;
          if(_d_)
           {if(80<_b_)
             {caml_call_gen3_j_(_c_[17],_bG_,0,80);var _b_=_b_-80|0;continue}
            var _e_=caml_call_gen3_j_(_c_[17],_bG_,0,_b_)}
          else
           var _e_=_d_;
          return _e_}};
      return _c_}
    function _bI_(_d_)
     {function _a_(_a_){return caml_ml_flush_b5_(_d_)}
      return _bH_
              (function(_a_,_b_,_c_)
                {if(0<=_b_&&0<=_c_&&!((_a_.getLen()-_c_|0)<_b_))
                  {var _e_=caml_ml_output_eO_(_d_,_a_,_b_,_c_),_f_=1}
                 else
                  var _f_=0;
                 if(!_f_)var _e_=_s_(_cI_);
                 return _e_},
               _a_)}
    function _aX_(_d_)
     {function _a_(_a_){return 0}
      return _bH_
              (function(_a_,_b_,_c_)
                {var _e_=_b_<0?1:0;
                 if(_e_)
                  var _f_=_e_;
                 else
                  {var _h_=_c_<0?1:0,_f_=_h_||((_a_.getLen()-_c_|0)<_b_?1:0)}
                 if(_f_)_s_(_cV_);
                 var _g_=_d_[2]+_c_|0;
                 if(_d_[3]<_g_)_aI_(_d_,_c_);
                 _Q_(_a_,_b_,_d_[1],_d_[2],_c_);
                 _d_[2]=_g_;
                 return 0},
               _a_)}
    var _dv_=_J_(_bb_),_dw_=_bI_(_cG_);
    _bI_(_cH_);
    _aX_(_dv_);
    function _bJ_(_a_,_b_,_c_)
     {if(_c_<_b_.getLen())
       {var _e_=_b_.safeGet(_c_),_d_=caml_call_gen1_h_(_aN_(_dx_),_e_)}
      else
       var _d_=caml_call_gen1_h_(_aN_(_dz_),46);
      var _f_=_aL_(_b_);
      return caml_call_gen4_b8_(_aN_(_dy_),_a_,_f_,_c_,_d_)}
    function _W_(_a_,_b_,_c_){return _s_(_bJ_(_a_,_b_,_c_))}
    function _al_(_a_,_b_){return _W_(_dA_,_a_,_b_)}
    function _B_(_a_,_b_){return _s_(_bJ_(_dB_,_a_,_b_))}
    function _bK_(_a_,_b_,_c_)
     {try
       {var _e_=caml_int_of_string_b7_(_c_),_d_=_e_}
      catch(_f_){if(_f_[1]!==_ab_)throw _f_;var _d_=_B_(_a_,_b_)}
      return _d_}
    function _bL_(_a_,_b_)
     {_aS_(_b_,0);
      var _c_=_af_(_a_);
      _a_[2]=0;
      _a_[1]=_a_[4];
      _a_[3]=_a_[1].getLen();
      return _c_}
    function _bM_(_a_,_b_)
     {var _c_=_J_(_bb_),_d_=_aX_(_c_);
      caml_call_gen2_n_(_a_,_d_,_b_);
      return _bL_(_c_,_d_)}
    function _bN_(_a_,_b_)
     {if(_b_)
       {var _e_=_ad_([0,_a_,_b_]);
        if(_e_)
         {var _g_=_e_[1],_i_=[0,0],_h_=[0,0],_k_=_e_[2];
          _f_
           (function(_a_){_i_[1]++;_h_[1]=_h_[1]+_a_.getLen()|0;return 0},_e_);
          var
           _d_=
            caml_create_string_x_
             (_h_[1]+caml_mul_a1_(_am_.getLen(),_i_[1]-1|0)|0);
          caml_blit_string_F_(_g_,0,_d_,0,_g_.getLen());
          var _c_=[0,_g_.getLen()];
          _f_
           (function(_a_)
             {caml_blit_string_F_(_am_,0,_d_,_c_[1],_am_.getLen());
              _c_[1]=_c_[1]+_am_.getLen()|0;
              caml_blit_string_F_(_a_,0,_d_,_c_[1],_a_.getLen());
              _c_[1]=_c_[1]+_a_.getLen()|0;
              return 0},
            _k_);
          var _j_=_d_}
        else
         var _j_=_cT_;
        return _j_}
      return _a_}
    function _aY_(_a_)
     {var _b_=_J_(_bb_);
      function _e_(_a_){return _bL_(_b_,_a_)}
      var _f_=_aX_(_b_),_p_=[0,0],_z_=1;
      function _H_(_a_)
       {var _c_=_p_[1];
        if(_c_){var _d_=_c_[1];_aT_(_f_,_d_,_A_(1,_a_));_p_[1]=0;return 0}
        var _b_=caml_create_string_x_(1);
        _b_.safeSet(0,_a_);
        return _bF_(_f_,1,_b_)}
      function _k_(_a_)
       {var _b_=_p_[1];
        return _b_
                ?(_aT_(_f_,_b_[1],_a_),_p_[1]=0,0)
                :_bF_(_f_,_a_.getLen(),_a_)}
      function _d_(_P_,_b_,_e_,_v_)
       {var _i_=_e_.getLen();
        function _g_(_a_,_b_)
         {var _d_=_b_;
          for(;;)
           {if(_i_<=_d_)return caml_call_gen1_h_(_P_,_f_);
            var _C_=_e_.safeGet(_d_);
            if(37===_C_)return _ah_(_e_,_v_,_a_,_d_,_Q_,_R_,_S_,_U_,_X_);
            if(64===_C_)
             {var _c_=_d_+1|0;
              if(_i_<=_c_)return _al_(_e_,_c_);
              var _j_=_e_.safeGet(_c_);
              if(65<=_j_)
               {if(94<=_j_)
                 {var _D_=_j_-_as_|0;
                  if(!(_D_<0||2<_D_))
                   switch(_D_)
                    {case 1:break;
                     case 2:
                      if(_f_[22])_V_(_f_,[0,0,5,0]);
                      if(_f_[21])
                       {var _B_=_f_[4];
                        if(_B_)
                         {var _O_=_B_[2];
                          caml_call_gen1_h_(_f_[26],_B_[1]);
                          _f_[4]=_O_;
                          var _N_=1}
                        else
                         var _N_=0}
                      else
                       var _N_=0;
                      var _d_=_c_+1|0;
                      continue;
                     default:
                      var _q_=_c_+1|0;
                      if(_i_<=_q_)
                       {_aR_(_f_,_dG_);var _E_=_g_(_a_,_q_)}
                      else
                       if(60===_e_.safeGet(_q_))
                        {var
                          _K_=
                           function(_a_,_b_,_c_)
                            {_aR_(_f_,_a_);return _g_(_b_,_x_(_c_))},
                          _M_=_q_+1|0,
                          _y_=
                           function(_f_,_b_,_c_,_d_)
                            {var _a_=_d_;
                             for(;;)
                              {if(_i_<=_a_)
                                return _K_(_bN_(_T_(_e_,_u_(_c_),_a_-_c_|0),_f_),_b_,_a_);
                               var _j_=_e_.safeGet(_a_);
                               if(37===_j_)
                                {var
                                  _g_=_T_(_e_,_u_(_c_),_a_-_c_|0),
                                  _k_=
                                   function(_a_,_b_,_c_)
                                    {return _y_([0,_b_,[0,_g_,_f_]],_a_,_c_,_c_)},
                                  _l_=
                                   function(_a_,_b_,_c_,_d_)
                                    {var _e_=_z_?caml_call_gen2_n_(_b_,0,_c_):_bM_(_b_,_c_);
                                     return _y_([0,_e_,[0,_g_,_f_]],_a_,_d_,_d_)},
                                  _m_=
                                   function(_a_,_d_,_c_)
                                    {if(_z_)
                                      var _b_=caml_call_gen1_h_(_d_,0);
                                     else
                                      {var
                                        _e_=0,
                                        _b_=
                                         _bM_
                                          (function(_a_,_b_){return caml_call_gen1_h_(_d_,_a_)},_e_)}
                                     return _y_([0,_b_,[0,_g_,_f_]],_a_,_c_,_c_)},
                                  _o_=function(_a_,_b_){return _W_(_dE_,_e_,_b_)};
                                 return _ah_
                                         (_e_,
                                          _v_,
                                          _b_,
                                          _a_,
                                          _k_,
                                          _l_,
                                          _m_,
                                          _o_,
                                          function(_a_,_b_,_c_){return _W_(_dF_,_e_,_c_)})}
                               if(62===_j_)
                                return _K_(_bN_(_T_(_e_,_u_(_c_),_a_-_c_|0),_f_),_b_,_a_);
                               var _a_=_a_+1|0;
                               continue}},
                          _E_=_y_(0,_a_,_M_,_M_)}
                       else
                        {_aR_(_f_,_dH_);var _E_=_g_(_a_,_q_)}
                      return _E_}}
                else
                 if(91<=_j_)
                  switch(_j_-91|0)
                   {case 1:break;
                    case 2:_bE_(_f_,0);var _d_=_c_+1|0;continue;
                    default:
                     var _r_=_c_+1|0;
                     if(_i_<=_r_)
                      {_ak_(_f_,0,4);var _F_=_g_(_a_,_r_)}
                     else
                      if(60===_e_.safeGet(_r_))
                       {var _l_=_r_+1|0;
                        if(_i_<=_l_)
                         var _k_=[0,4,_l_];
                        else
                         {var _G_=_e_.safeGet(_l_);
                          if(98===_G_)
                           var _k_=[0,4,_l_+1|0];
                          else
                           if(104===_G_)
                            {var _m_=_l_+1|0;
                             if(_i_<=_m_)
                              var _k_=[0,0,_m_];
                             else
                              {var _I_=_e_.safeGet(_m_);
                               if(_au_===_I_)
                                {var _t_=_m_+1|0;
                                 if(_i_<=_t_)
                                  var _k_=_W_(_dC_,_e_,_t_);
                                 else
                                  {var
                                    _J_=_e_.safeGet(_t_),
                                    _k_=
                                     _a9_===_J_?[0,3,_t_+1|0]:_W_(_o_(_dD_,_A_(1,_J_)),_e_,_t_)}}
                               else
                                var _k_=_a9_===_I_?[0,2,_m_+1|0]:[0,0,_m_]}}
                           else
                            var _k_=_a9_===_G_?[0,1,_l_+1|0]:[0,4,_l_]}
                        var
                         _Z_=_k_[2],
                         ___=_k_[1],
                         _F_=
                          _w_
                           (_a_,
                            _Z_,
                            function(_a_,_b_,_c_)
                             {_ak_(_f_,_a_,___);return _g_(_b_,_x_(_c_))})}
                      else
                       {_ak_(_f_,0,4);var _F_=_g_(_a_,_r_)}
                     return _F_}}
              else
               {if(10===_j_)
                 {if(_f_[14]<_f_[15])_bz_(_f_,_L_(0,3,0));
                  var _d_=_c_+1|0;
                  continue}
                if(32<=_j_)
                 switch(_j_-32|0)
                  {case 0:_aW_(_f_,0);var _d_=_c_+1|0;continue;
                   case 12:_aV_(_f_,0,0);var _d_=_c_+1|0;continue;
                   case 14:
                    _aS_(_f_,1);
                    caml_call_gen1_h_(_f_[18],0);
                    var _d_=_c_+1|0;
                    continue;
                   case 27:
                    var
                     _s_=_c_+1|0,
                     _Y_=
                      _i_<=_s_
                       ?(_aW_(_f_,0),_g_(_a_,_s_))
                       :60===_e_.safeGet(_s_)
                         ?_w_
                           (_a_,
                            _s_+1|0,
                            function(_d_,_b_,_c_)
                             {return _w_
                                      (_b_,
                                       _c_,
                                       function(_a_,_b_,_c_)
                                        {_aV_(_f_,_d_,_a_);return _g_(_b_,_x_(_c_))})})
                         :(_aW_(_f_,0),_g_(_a_,_s_));
                    return _Y_;
                   case 28:
                    return _w_
                            (_a_,
                             _c_+1|0,
                             function(_a_,_b_,_c_)
                              {_p_[1]=[0,_a_];return _g_(_b_,_x_(_c_))});
                   case 31:_aU_(_f_,0);var _d_=_c_+1|0;continue;
                   case 32:_H_(64);var _d_=_c_+1|0;continue;
                   default:}}
              return _al_(_e_,_c_)}
            _H_(_C_);
            var _d_=_d_+1|0;
            continue}}
        function _Q_(_a_,_b_,_c_){_k_(_b_);return _g_(_a_,_c_)}
        function _R_(_a_,_b_,_c_,_d_)
         {if(_z_)
           _k_(caml_call_gen2_n_(_b_,0,_c_));
          else
           caml_call_gen2_n_(_b_,_f_,_c_);
          return _g_(_a_,_d_)}
        function _S_(_a_,_b_,_c_)
         {if(_z_)
           _k_(caml_call_gen1_h_(_b_,0));
          else
           caml_call_gen1_h_(_b_,_f_);
          return _g_(_a_,_c_)}
        function _U_(_a_,_b_){_aU_(_f_,0);return _g_(_a_,_b_)}
        function _X_(_a_,_b_,_c_)
         {var _e_=_aK_(_aM_(_b_),_a_);
          return _d_(function(_a_){return _g_(_e_,_c_)},_a_,_b_,_v_)}
        function _w_(_a_,_b_,_g_)
         {var _c_=_b_;
          for(;;)
           {if(_i_<=_c_)return _B_(_e_,_c_);
            var _h_=_e_.safeGet(_c_);
            if(32===_h_){var _c_=_c_+1|0;continue}
            if(37===_h_)
             {var
               _l_=
                function(_a_,_b_,_c_)
                 {return caml_call_gen3_j_(_g_,_bK_(_e_,_c_,_b_),_a_,_c_)},
               _m_=function(_a_,_b_,_c_,_d_){return _B_(_e_,_d_)},
               _n_=function(_a_,_b_,_c_){return _B_(_e_,_c_)},
               _o_=function(_a_,_b_){return _B_(_e_,_b_)};
              return _ah_
                      (_e_,
                       _v_,
                       _a_,
                       _c_,
                       _l_,
                       _m_,
                       _n_,
                       _o_,
                       function(_a_,_b_,_c_){return _B_(_e_,_c_)})}
            var _d_=_c_;
            for(;;)
             {if(_i_<=_d_)
               var _k_=_B_(_e_,_d_);
              else
               {var _f_=_e_.safeGet(_d_),_q_=48<=_f_?58<=_f_?0:1:45===_f_?1:0;
                if(_q_){var _d_=_d_+1|0;continue}
                var
                 _p_=_d_===_c_?0:_bK_(_e_,_d_,_T_(_e_,_u_(_c_),_d_-_c_|0)),
                 _k_=caml_call_gen3_j_(_g_,_p_,_a_,_d_)}
              return _k_}}}
        function _x_(_a_)
         {var _b_=_a_;
          for(;;)
           {if(_i_<=_b_)return _al_(_e_,_b_);
            var _c_=_e_.safeGet(_b_);
            if(32===_c_){var _b_=_b_+1|0;continue}
            return 62===_c_?_b_+1|0:_al_(_e_,_b_)}}
        return _g_(_b_,0)}
      var _c_=_u_(0);
      return _bp_(function(_a_,_b_){return _d_(_e_,_c_,_a_,_b_)},_a_)}
    var _cJ_=_aE_[1];
    _aE_[1]=function(_a_){_aU_(_dw_,0);return caml_call_gen1_h_(_cJ_,0)};
    var _e_=joo_global_object_c_,_bO_=null,_bP_=undefined;
    function _bQ_(_a_){return 1-(_a_==_bO_?1:0)}
    var _M_=true,_C_=false,_bR_=Date,_dI_=Array;
    function _dJ_(_a_)
     {return _a_ instanceof _dI_?0:[0,new MlWrappedString_$_(_a_.toString())]}
    _bs_[1]=[0,_dJ_,_bs_[1]];
    function _aZ_(_a_,_b_){_a_.appendChild(_b_);return 0}
    var
     _an_=_e_.document,
     _dK_=_e_.Int8Array,
     _dL_=_e_.Uint8Array,
     _dM_=_e_.Int16Array,
     _dN_=_e_.Int32Array,
     _dO_=_e_.Float32Array;
    function _a0_(_a_,_b_){return _a_.createElement(_b_.toString())}
    var _dS_=[0,_dR_];
    _e_.HTMLElement===_bP_;
    var
     _l_=caml_js_get_console_eF_(0),
     _dU_=
      caml_js_pure_expr_eG_
       (function(_a_)
         {var
           _h_=
            [0,
             _e_.requestAnimationFrame,
             [0,
              _e_.mozRequestAnimationFrame,
              [0,
               _e_.webkitRequestAnimationFrame,
               [0,
                _e_.oRequestAnimationFrame,
                [0,_e_.msRequestAnimationFrame,0]]]]];
          try
           {var _b_=_h_;
            for(;;)
             {if(!_b_)throw [0,_P_];
              var _c_=_b_[1],_f_=_b_[2];
              if(_c_===_bP_){var _b_=_f_;continue}
              var _i_=function(_a_){return _c_(_a_)};
              break}}
          catch(_f_)
           {if(_f_[1]===_P_)
             {var
               _d_=function(_a_){return new _bR_().getTime()},
               _g_=[0,_d_(0)];
              return function(_a_)
               {var _b_=_d_(0),_c_=_g_[1]+_a__/60-_b_,_f_=_c_<0?0:_c_;
                _g_[1]=_b_;
                _e_.setTimeout(_a_,_f_);
                return 0}}
            throw _f_}
          return _i_}),
     _E_=_a0_(_an_,_dP_);
    _E_.style.position="absolute";
    _E_.style.top=_b__;
    _E_.style.left=_b__;
    _E_.style.lineHeight="0.9em";
    _E_.style.color="red";
    var _bS_=_a0_(_an_,_dQ_);
    _aZ_(_E_,_bS_);
    var _bT_=1;
    function _N_(_a_){return new _bR_().getTime()}
    var _X_=1;
    function _bU_(_a_,_b_)
     {var _c_=_a_.createBuffer();
      _a_.bindBuffer(_a_.ARRAY_BUFFER,_c_);
      var _d_=new _dO_(caml_js_from_array_G_(_b_));
      _a_.bufferData(_a_.ARRAY_BUFFER,_d_,_a_.STATIC_DRAW);
      return _c_}
    function _v_(_a_,_b_,_c_,_d_,_e_,_f_,_g_,_h_)
     {var
       _j_=_e_?_e_[1]:0,
       _k_=_f_?_f_[1]:_a_.FLOAT,
       _l_=_g_?_g_[1]:_g_,
       _i_=_a_.getAttribLocation(_b_,_c_.toString());
      _a_.enableVertexAttribArray(_i_);
      _a_.bindBuffer(_a_.ARRAY_BUFFER,_h_);
      return _a_.vertexAttribPointer(_i_,_d_,_k_,!!_l_,0,_j_)}
    function _bV_(_a_,_b_,_c_)
     {var _e_=_a_.createShader(_a_.VERTEX_SHADER);
      _a_.shaderSource(_e_,_b_.toString());
      _a_.compileShader(_e_);
      var _f_=_a_.createShader(_a_.FRAGMENT_SHADER);
      _a_.shaderSource(_f_,_c_.toString());
      _a_.compileShader(_f_);
      var _d_=_a_.createProgram();
      _a_.attachShader(_d_,_e_);
      _a_.attachShader(_d_,_f_);
      _a_.linkProgram(_d_);
      if(1-(_a_.getShaderParameter(_e_,_a_.COMPILE_STATUS)|0))
       _l_.log(_a_.getShaderInfoLog(_e_));
      if(1-(_a_.getShaderParameter(_f_,_a_.COMPILE_STATUS)|0))
       _l_.log(_a_.getShaderInfoLog(_f_));
      if(1-(_a_.getProgramParameter(_d_,_a_.LINK_STATUS)|0))
       _l_.log(_a_.getProgramInfoLog(_d_));
      _a_.useProgram(_d_);
      return _d_}
    function _bW_(_a_)
     {var _j_=_a0_(_e_.document,_dT_),_ap_=600,_aq_=_a__;
      if(_bQ_(_j_.getContext))
       {_j_.width=_aq_;
        _j_.height=_ap_;
        _aZ_(_an_.body,_j_);
        var
         _ar_=function(_a_){throw [0,_aF_,_d2_]},
         _Q_=
          _j_.getContext("experimental-webgl",{"antialias":_M_,"stencil":_M_}),
         _b_=_Q_==_bO_?_ar_(0):_Q_;
        _l_.log(_b_.getContextAttributes());
        var _R_=_bQ_(_b_.getExtension("OES_element_index_uint"));
        _b_.viewport(0,0,_j_.width,_j_.height);
        if(_X_)_b_.clearColor(1,1,1,1);else _b_.clearColor(0,0,0,0);
        var _w_=_bV_(_b_,_dV_,_dW_),_as_=_b_.getUniformLocation(_w_,_b$_);
        _b_.uniform2f(_as_,2/_j_.width,2/_j_.height);
        var _m_=_bV_(_b_,_dX_,_dY_),_at_=_b_.getUniformLocation(_m_,_b$_);
        _b_.uniform2f(_at_,2/_j_.width,2/_j_.height);
        if(_X_)
         _b_.blendFunc(_b_.ONE,_b_.ONE_MINUS_SRC_ALPHA);
        else
         _b_.blendFunc(_b_.ONE_MINUS_DST_ALPHA,_b_.ONE);
        _b_.enable(_b_.BLEND);
        var
         _B_=[0,_ee_],
         _r_=[0,0],
         _x_=[0,0],
         _s_=[0,0],
         _D_=[0,0],
         _S_=[0,0],
         _T_=
          function(_a_)
           {var _E_=0!==_x_[1]?1:0;
            if(_E_)
             {var
               _F_=_x_[1],
               _T_=_s_[1],
               _j_=_X_?_ad_(_F_):_F_,
               _K_=0,
               _m_=_bj_(function(_a_,_b_){return _a_+_aG_(_b_)|0},_K_,_j_),
               _u_=_m_-_aG_(_j_)|0;
              _l_.log(caml_call_gen1_h_(_aY_(_dZ_),_m_).toString());
              _l_.log(caml_call_gen1_h_(_aY_(_d0_),_u_).toString());
              var _i_=caml_make_vect_k_((8*_m_|0)+16|0,0),_v_=[0,8];
              _f_
               (function(_a_)
                 {return _f_
                          (function(_a_)
                            {var _c_=_a_[2],_d_=_a_[1],_b_=_v_[1];
                             _v_[1]=_b_+8|0;
                             caml_array_set_g_(_i_,_b_+0|0,_d_);
                             caml_array_set_g_(_i_,_b_+1|0,_c_);
                             caml_array_set_g_(_i_,_b_+2|0,_d_);
                             caml_array_set_g_(_i_,_b_+3|0,_c_);
                             caml_array_set_g_(_i_,_b_+4|0,_d_);
                             caml_array_set_g_(_i_,_b_+5|0,_c_);
                             caml_array_set_g_(_i_,_b_+6|0,_d_);
                             return caml_array_set_g_(_i_,_b_+7|0,_c_)},
                           _a_)},
                _j_);
              var _c_=caml_make_vect_k_(16*_m_|0,0);
              _l_.log("cols",_c_.length-1);
              var _w_=[0,-2*4|0];
              _f_
               (function(_a_)
                 {return _f_
                          (function(_a_)
                            {var _e_=_a_[6],_f_=_a_[5],_i_=0,_j_=1;
                             if(!(1<0))
                              {var _h_=_i_;
                               for(;;)
                                {var _b_=_w_[1];
                                 _w_[1]=_b_+8|0;
                                 if(0<=_b_)
                                  {caml_array_set_g_(_c_,_b_+0|0,caml_array_get_d_(_f_,0));
                                   caml_array_set_g_(_c_,_b_+1|0,caml_array_get_d_(_f_,1));
                                   caml_array_set_g_(_c_,_b_+2|0,caml_array_get_d_(_f_,2));
                                   caml_array_set_g_(_c_,_b_+3|0,caml_array_get_d_(_f_,3));
                                   caml_array_set_g_(_c_,_b_+4|0,caml_array_get_d_(_e_,0));
                                   caml_array_set_g_(_c_,_b_+5|0,caml_array_get_d_(_e_,1));
                                   caml_array_set_g_(_c_,_b_+6|0,caml_array_get_d_(_e_,2));
                                   caml_array_set_g_(_c_,_b_+7|0,caml_array_get_d_(_e_,3))}
                                 var _k_=_h_+1|0;
                                 if(_j_!==_h_){var _h_=_k_;continue}
                                 break}}
                             return 0},
                           _a_)},
                _j_);
              var _n_=caml_make_vect_k_(caml_mul_a1_(4*4|0,_m_),1),_y_=[0,-2];
              _f_
               (function(_a_)
                 {return _f_
                          (function(_a_)
                            {var _d_=8*_a_[4]+_bc_|0;
                             if(_cr_<=_d_)throw [0,_aF_,_d1_];
                             var _c_=_y_[1];
                             _y_[1]=_c_+4|0;
                             var _e_=0<=_c_?0:2,_f_=3;
                             if(!(3<_e_))
                              {var _b_=_e_;
                               for(;;)
                                {caml_array_set_g_(_n_,4*(_c_+_b_|0)|0,_d_);
                                 var _h_=_b_+1|0;
                                 if(_f_!==_b_){var _b_=_h_;continue}
                                 break}}
                             return 0},
                           _a_)},
                _j_);
              var _z_=[0,0];
              _f_
               (function(_a_)
                 {return _f_
                          (function(_a_)
                            {var _b_=_a_[3],_d_=_z_[1];
                             _z_[1]=_d_+4|0;
                             var
                              _e_=
                               _cq_<=_b_
                                ?_cm_<=_b_?_ce_<=_b_?1:-1:_cj_<=_b_?-2:3
                                :_cf_<=_b_?2:-3,
                              _f_=0,
                              _h_=3;
                             if(!(3<0))
                              {var _c_=_f_;
                               for(;;)
                                {caml_array_set_g_(_n_,(4*(_d_+_c_|0)|0)+1|0,_e_);
                                 var _i_=_c_+1|0;
                                 if(_h_!==_c_){var _c_=_i_;continue}
                                 break}}
                             return 0},
                           _a_)},
                _j_);
              var _A_=_m_-2|0,_L_=0;
              if(!(_A_<0))
               {var _p_=_L_;
                for(;;)
                 {caml_array_set_g_(_n_,(4*((4*_p_|0)+1|0)|0)+2|0,-1);
                  caml_array_set_g_(_n_,(4*((4*_p_|0)+3|0)|0)+2|0,-1);
                  var _S_=_p_+1|0;
                  if(_A_!==_p_){var _p_=_S_;continue}
                  break}}
              var _B_=_m_-2|0,_M_=0;
              if(!(_B_<0))
               {var _o_=_M_;
                for(;;)
                 {caml_array_set_g_(_n_,(4*((4*_o_|0)+2|0)|0)+3|0,-1);
                  caml_array_set_g_(_n_,(4*((4*_o_|0)+3|0)|0)+3|0,-1);
                  var _Q_=_o_+1|0;
                  if(_B_!==_o_){var _o_=_Q_;continue}
                  break}}
              var _e_=caml_make_vect_k_(6*_u_|0,0),_C_=[0,0],_q_=[0,0];
              _f_
               (function(_a_)
                 {var _f_=(_aG_(_a_)-1|0)-1|0,_h_=0;
                  if(!(_f_<0))
                   {var _d_=_h_;
                    for(;;)
                     {var _b_=_C_[1];
                      _C_[1]=_b_+6|0;
                      var _c_=_q_[1];
                      _q_[1]=_c_+4|0;
                      caml_array_set_g_(_e_,_b_+0|0,_c_+0|0);
                      caml_array_set_g_(_e_,_b_+1|0,_c_+1|0);
                      caml_array_set_g_(_e_,_b_+2|0,_c_+2|0);
                      caml_array_set_g_(_e_,_b_+3|0,_c_+2|0);
                      caml_array_set_g_(_e_,_b_+4|0,_c_+1|0);
                      caml_array_set_g_(_e_,_b_+5|0,_c_+3|0);
                      var _i_=_d_+1|0;
                      if(_f_!==_d_){var _d_=_i_;continue}
                      break}}
                  _q_[1]=_q_[1]+4|0;
                  return 0},
                _j_);
              _l_.log(_e_.length-1);
              var _N_=_bU_(_b_,_i_),_r_=_b_.createBuffer();
              _b_.bindBuffer(_b_.ARRAY_BUFFER,_r_);
              var _I_=new _dL_(caml_js_from_array_G_(_c_));
              _b_.bufferData(_b_.ARRAY_BUFFER,_I_,_b_.STATIC_DRAW);
              var _t_=_b_.createBuffer();
              _b_.bindBuffer(_b_.ARRAY_BUFFER,_t_);
              var _J_=new _dK_(caml_js_from_array_G_(_n_));
              _b_.bufferData(_b_.ARRAY_BUFFER,_J_,_b_.STATIC_DRAW);
              var _D_=_b_.createBuffer();
              _b_.bindBuffer(_b_.ELEMENT_ARRAY_BUFFER,_D_);
              if(_R_)
               {var _O_=new _dN_(caml_js_from_array_G_(_e_));
                _b_.bufferData(_b_.ELEMENT_ARRAY_BUFFER,_O_,_b_.STATIC_DRAW)}
              else
               {var _P_=new _dM_(caml_js_from_array_G_(_e_));
                _b_.bufferData(_b_.ELEMENT_ARRAY_BUFFER,_P_,_b_.STATIC_DRAW)}
              _s_[1]=[0,[0,137317855,[0,_N_,_r_,_t_,_e_.length-1,_D_]],_T_];
              _x_[1]=0;
              var _H_=0}
            else
             var _H_=_E_;
            return _H_},
         _F_=
          function(_a_)
           {if(0!==_r_[1])
             {var
               _I_=_r_[1],
               _J_=0,
               _n_=
                _bj_(function(_a_,_b_){return _a_+(_b_.length-1)|0},_J_,_I_),
               _c_=caml_make_vect_k_(_n_,0),
               _F_=[0,0],
               _K_=_r_[1];
              _f_
               (function(_a_)
                 {_S_[1]++;
                  var _e_=_F_[1];
                  _F_[1]=_e_+(_a_.length-1)|0;
                  var _f_=_a_.length-1-1|0,_h_=0;
                  if(!(_f_<0))
                   {var _b_=_h_;
                    for(;;)
                     {caml_array_set_g_(_c_,_e_+_b_|0,caml_array_get_d_(_a_,_b_));
                      var _i_=_b_+1|0;
                      if(_f_!==_b_){var _b_=_i_;continue}
                      break}}
                  return 0},
                _K_);
              _D_[1]=_D_[1]+_n_|0;
              _l_.log(_n_);
              var
               _L_=_s_[1],
               _M_=_bU_(_b_,_c_),
               _o_=caml_array_get_d_(_c_,0),
               _p_=caml_array_get_d_(_c_,0),
               _q_=caml_array_get_d_(_c_,1),
               _t_=caml_array_get_d_(_c_,1),
               _u_=((_c_.length-1)/2|0)-1|0,
               _N_=(_c_.length-1)/2|0,
               _G_=0;
              if(_u_<0)
               {var _y_=_t_,_x_=_q_,_w_=_p_,_v_=_o_}
              else
               {var _e_=_G_,_m_=_t_,_j_=_q_,_i_=_p_,_h_=_o_;
                for(;;)
                 {var
                   _z_=
                    caml_lessthan_a3_(caml_array_get_d_(_c_,2*_e_|0),_h_)
                     ?caml_array_get_d_(_c_,2*_e_|0)
                     :_h_,
                   _A_=
                    caml_greaterthan_ca_(caml_array_get_d_(_c_,2*_e_|0),_i_)
                     ?caml_array_get_d_(_c_,2*_e_|0)
                     :_i_,
                   _C_=
                    caml_lessthan_a3_(caml_array_get_d_(_c_,(2*_e_|0)+1|0),_j_)
                     ?caml_array_get_d_(_c_,(2*_e_|0)+1|0)
                     :_j_,
                   _E_=
                    caml_greaterthan_ca_
                      (caml_array_get_d_(_c_,(2*_e_|0)+1|0),_m_)
                     ?caml_array_get_d_(_c_,(2*_e_|0)+1|0)
                     :_m_,
                   _H_=_e_+1|0;
                  if(_u_!==_e_)
                   {var _e_=_H_,_m_=_E_,_j_=_C_,_i_=_A_,_h_=_z_;continue}
                  var _y_=_E_,_x_=_C_,_w_=_A_,_v_=_z_;
                  break}}
              _s_[1]=[0,[0,_cd_,[0,_B_[1],[0,_v_,_x_,_w_,_y_],_N_,_M_]],_L_]}
            _r_[1]=0;
            return 0},
         _H_=scene,
         _au_=_N_(0),
         _t_=_j_.height;
        _l_.log("scene:",_H_.length-1);
        var _U_=_H_.length-1-1|0,_av_=0;
        if(!(_U_<0))
         {var _z_=_av_;
          for(;;)
           {var _c_=caml_array_get_d_(_H_,_z_);
            if(caml_array_get_d_(_c_,0)==1)
             {var
               _Z_=(_c_.length-1-4|0)/2|0,
               _q_=caml_make_vect_k_(6*(_Z_-2|0)|0,0),
               ___=_Z_-3|0,
               _ax_=caml_array_get_d_(_c_,1),
               _ay_=caml_array_get_d_(_c_,2),
               _az_=caml_array_get_d_(_c_,3),
               _aA_=0;
              if(!(___<0))
               {var _i_=_aA_;
                for(;;)
                 {caml_array_set_g_
                   (_q_,(6*_i_|0)+0|0,caml_array_get_d_(_c_,4));
                  caml_array_set_g_
                   (_q_,(6*_i_|0)+1|0,_t_-caml_array_get_d_(_c_,5));
                  caml_array_set_g_
                   (_q_,(6*_i_|0)+2|0,caml_array_get_d_(_c_,(2*_i_|0)+6|0));
                  caml_array_set_g_
                   (_q_,(6*_i_|0)+3|0,_t_-caml_array_get_d_(_c_,(2*_i_|0)+7|0));
                  caml_array_set_g_
                   (_q_,(6*_i_|0)+4|0,caml_array_get_d_(_c_,(2*_i_|0)+8|0));
                  caml_array_set_g_
                   (_q_,(6*_i_|0)+5|0,_t_-caml_array_get_d_(_c_,(2*_i_|0)+9|0));
                  var _aC_=_i_+1|0;
                  if(___!==_i_){var _i_=_aC_;continue}
                  break}}
              var _$_=[0,_ax_,_ay_,_az_,1];
              _T_(0);
              if(caml_notequal_eT_(_$_,_B_[1])){_F_(0);_B_[1]=_$_}
              _r_[1]=[0,_q_,_r_[1]]}
            else
             {_F_(0);
              var
               _O_=caml_array_get_d_(_c_,4),
               _aD_=caml_array_get_d_(_c_,1),
               _aE_=caml_array_get_d_(_c_,2),
               _aH_=caml_array_get_d_(_c_,3),
               _A_=function(_a_){return _a_*255.99|0},
               _aI_=_A_(1),
               _aJ_=_A_(_aH_),
               _aK_=_A_(_aE_),
               _u_=[0,_A_(_aD_),_aK_,_aJ_,_aI_],
               _aa_=caml_array_get_d_(_c_,5)==1?1:0,
               _ab_=caml_array_get_d_(_c_,6),
               _aL_=_ab_!=1?_ab_!=2?-752251131:_cj_:_cm_,
               _ae_=caml_array_get_d_(_c_,7),
               _af_=_ae_!=1?_ae_!=2?_cq_:_cf_:_ce_,
               _ag_=
                [0,
                 caml_array_get_d_(_c_,8),
                 _t_-caml_array_get_d_(_c_,9),
                 _af_,
                 _O_,
                 _u_,
                 _u_],
               _ah_=[0,_ag_,0],
               _aj_=0,
               _aM_=_aa_?1:2,
               _ak_=((_c_.length-1-10|0)/2|0)-_aM_|0;
              if(_ak_<_aj_)
               var _P_=_ah_;
              else
               {var _y_=_aj_,_am_=_ah_;
                for(;;)
                 {var
                   _ao_=
                    [0,
                     [0,
                      caml_array_get_d_(_c_,(2*_y_|0)+10|0),
                      _t_-caml_array_get_d_(_c_,(2*_y_|0)+11|0),
                      _aL_,
                      _O_,
                      _u_,
                      _u_],
                     _am_],
                   _aO_=_y_+1|0;
                  if(_ak_!==_y_){var _y_=_aO_,_am_=_ao_;continue}
                  var _P_=_ao_;
                  break}}
              var
               _al_=_c_.length-1,
               _aN_=
                _aa_
                 ?[0,_ag_,_P_]
                 :[0,
                   [0,
                    caml_array_get_d_(_c_,_al_-2|0),
                    _t_-caml_array_get_d_(_c_,_al_-1|0),
                    _af_,
                    _O_,
                    _u_,
                    _u_],
                   _P_];
              _x_[1]=[0,_aN_,_x_[1]]}
            var _aB_=_z_+1|0;
            if(_U_!==_z_){var _z_=_aB_;continue}
            break}}
        _F_(0);
        _T_(0);
        _l_.log("polygons:",_D_[1],_S_[1]);
        _l_.log("preparation:",_N_(0)-_au_);
        var
         _aw_=_X_?_ad_(_s_[1]):_s_[1],
         _I_=[0,_ef_.slice()],
         _J_=[0,0],
         _V_=[0,_N_(0)],
         _K_=[0,15],
         _L_=[0,15],
         _W_=[0,0],
         _Y_=
          function(_a_)
           {var _q_=_N_(0);
            _K_[1]=_cn_*_K_[1]+_cg_*(_q_-_V_[1]);
            _W_[1]++;
            if(0===(_W_[1]%60|0))
             {var
               _P_=_L_[1],
               _Q_=_a__/_K_[1],
               _S_=caml_call_gen2_n_(_aY_(_eg_),_Q_,_P_);
              _aZ_(_an_.body,_E_);
              _bS_.innerHTML=_S_.toString()}
            _V_[1]=_q_;
            if(_bT_)caml_call_gen1_h_(_dU_,caml_js_wrap_callback_cb_(_Y_));
            _J_[1]=_J_[1]+1;
            var
             _T_=_bc_*(1.8+0.8*Math.cos(_J_[1]*3.14/180)),
             _j_=_bT_?_T_:_bc_;
            _I_[1]=[_cs_,_j_,0,0,0,_j_,0,500*(1-_j_),300*(1-_j_),1];
            _N_(0);
            var
             _y_=[0,0],
             _s_=0===_y_.length-1?[0,0]:_y_,
             _k_=_s_.length-1,
             _z_=0,
             _A_=54;
            if(!(54<0))
             {var _e_=_z_;
              for(;;)
               {caml_array_set_g_(_ai_[1],_e_,_e_);
                var _O_=_e_+1|0;
                if(_A_!==_e_){var _e_=_O_;continue}
                break}}
            var
             _l_=[0,_dg_],
             _t_=0,
             _B_=55,
             _D_=caml_greaterequal_b9_(55,_k_)?_B_:_k_,
             _u_=54+_D_|0;
            if(!(_u_<_t_))
             {var _c_=_t_;
              for(;;)
               {var
                 _x_=_c_%55|0,
                 _F_=_l_[1],
                 _r_=
                  _o_(_F_,_ac_(caml_array_get_d_(_s_,caml_mod_eP_(_c_,_k_))));
                _l_[1]=caml_md5_string_eL_(_r_,0,_r_.getLen());
                var _i_=_l_[1];
                caml_array_set_g_
                 (_ai_[1],
                  _x_,
                  (caml_array_get_d_(_ai_[1],_x_)^
                   (((_i_.safeGet(0)+(_i_.safeGet(1)<<8)|0)+
                     (_i_.safeGet(2)<<16)|
                     0)+
                    (_i_.safeGet(3)<<24)|
                    0))&
                  1073741823);
                var _H_=_c_+1|0;
                if(_u_!==_c_){var _c_=_H_;continue}
                break}}
            _ai_[2]=0;
            _b_.clear
             (_b_.COLOR_BUFFER_BIT|_b_.STENCIL_BUFFER_BIT|_b_.DEPTH_BUFFER_BIT);
            _f_
             (function(_a_)
               {if(_cd_<=_a_[1])
                 {var
                   _i_=_a_[2],
                   _n_=_i_[3],
                   _h_=_i_[1],
                   _c_=_h_[4],
                   _x_=_i_[4],
                   _y_=_I_[1];
                  _b_.useProgram(_w_);
                  var _o_=_b_.getUniformLocation(_w_,_cc_);
                  _b_.uniformMatrix3fv(_o_,_C_,caml_js_from_array_G_(_y_));
                  var _q_=_b_.getUniformLocation(_w_,_b2_);
                  _b_.uniform4f(_q_,_h_[1]*_c_,_h_[2]*_c_,_h_[3]*_c_,_c_);
                  _v_(_b_,_w_,_d3_,2,0,0,0,_x_);
                  _b_.enable(_b_.STENCIL_TEST);
                  _b_.disable(_b_.CULL_FACE);
                  _b_.colorMask(_C_,_C_,_C_,_C_);
                  _b_.stencilFunc(_b_.ALWAYS,0,_p_);
                  _b_.stencilOpSeparate
                   (_b_.FRONT,_b_.INCR_WRAP,_b_.INCR_WRAP,_b_.INCR_WRAP);
                  _b_.stencilOpSeparate
                   (_b_.BACK,_b_.DECR_WRAP,_b_.DECR_WRAP,_b_.DECR_WRAP);
                  _b_.drawArrays(_b_.TRIANGLES,0,_n_);
                  _b_.enable(_b_.CULL_FACE);
                  var _j_=1-_X_,_k_=_j_?_c_<1?1:0:_j_;
                  if(_k_)_b_.enable(_b_.DEPTH_TEST);
                  _b_.cullFace(_b_.FRONT);
                  _b_.colorMask(_M_,_M_,_M_,_M_);
                  _b_.stencilFunc(_b_.NOTEQUAL,0,_p_);
                  _b_.stencilOp(_b_.REPLACE,_b_.REPLACE,_b_.REPLACE);
                  _b_.drawArrays(_b_.TRIANGLES,0,_n_);
                  _b_.disable(_b_.CULL_FACE);
                  if(_k_)_b_.disable(_b_.DEPTH_TEST);
                  return _b_.disable(_b_.STENCIL_TEST)}
                var _e_=_a_[2],_f_=_I_[1],_l_=_e_[3],_g_=_e_[1];
                _b_.useProgram(_m_);
                _v_(_b_,_m_,_d5_,2,_d4_,0,0,_g_);
                _v_(_b_,_m_,_d7_,2,_d6_,0,0,_g_);
                _v_(_b_,_m_,_d9_,2,_d8_,0,0,_g_);
                _v_(_b_,_m_,_d$_,2,_d__,0,0,_g_);
                _v_(_b_,_m_,_eb_,4,0,[0,_b_.UNSIGNED_BYTE],_ea_,_e_[2]);
                _v_(_b_,_m_,_ec_,4,0,[0,_b_.BYTE],0,_l_);
                _v_(_b_,_m_,_ed_,4,[0,4*4|0],[0,_b_.BYTE],0,_l_);
                var _r_=_b_.getUniformLocation(_m_,_cc_);
                _b_.uniformMatrix3fv(_r_,_C_,caml_js_from_array_G_(_f_));
                var
                 _s_=
                  Math.sqrt
                   (caml_array_get_d_(_f_,0)*
                    caml_array_get_d_(_f_,4)-
                    caml_array_get_d_(_f_,1)*
                    caml_array_get_d_(_f_,3));
                _b_.uniform1f(_b_.getUniformLocation(_m_,"zoom"),_s_);
                _b_.bindBuffer(_b_.ELEMENT_ARRAY_BUFFER,_e_[5]);
                var _t_=0,_u_=_R_?_b_.UNSIGNED_INT:_b_.UNSIGNED_SHORT;
                return _b_.drawElements(_b_.TRIANGLES,_e_[4],_u_,_t_)},
              _aw_);
            _b_.finish();
            var _U_=_cg_*(_N_(0)-_q_);
            _L_[1]=_cn_*_L_[1]+_U_;
            return 0};
        _Y_(0);
        return _C_}
      throw [0,_dS_]}
    _e_.onload=
    caml_js_wrap_callback_cb_
     (function(_a_)
       {if(_a_){var _d_=_bW_(_a_);if(!(_d_|0))_a_.preventDefault();return _d_}
        var _c_=event,_b_=_bW_(_c_);
        if(!(_b_|0))_c_.returnValue=_b_;
        return _b_});
    _bi_(0);
    return}
  (this));

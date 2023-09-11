defmodule Unifex.CodeGenerator.BaseTypes.List do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for lists.

  They are represented in the native code as arrays with sizes passed
  via separate arguments.

  Implemented both for NIF and CNode as function parameter as well as return type.
  """
  use Unifex.CodeGenerator.BaseType
  alias Unifex.CodeGenerator.BaseType

  @impl true
  def ptr_level(ctx) do
    BaseType.ptr_level(ctx.subtype, ctx.generator, ctx) + 1
  end

  @impl true
  def generate_native_type(ctx) do
    optional_const = if ctx.mode == :const, do: "const ", else: ""

    [
      "#{BaseType.generate_native_type(ctx.subtype, ctx.mode, ctx.generator, ctx)} #{optional_const}*",
      {"unsigned int", "_length"}
    ]
  end

  @impl true
  def generate_initialization(name, _ctx) do
    ~g<#{name} = NULL;>
  end

  @impl true
  def generate_destruction(name, ctx) do
    ~g"""
    if(#{name} != NULL) {
      for(unsigned int i = 0; i < #{name}_length; i++) {
        #{BaseType.generate_destruction(ctx.subtype, :"#{name}[i]", ctx.generator, ctx)}
      }
      unifex_free(#{name});
    }
    """
  end

  defmodule NIF do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType
    alias Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, ctx) do
      ~g"""
      ({
        ERL_NIF_TERM list = enif_make_list(env, 0);
        for(int i = #{name}_length-1; i >= 0; i--) {
          list = enif_make_list_cell(
            env,
            #{BaseType.generate_arg_serialize(ctx.subtype, :"#{name}[i]", ctx.generator, ctx)},
            list
          );
        }
        list;
      })
      """
    end

    @impl true
    def generate_arg_parse(arg, var_name, ctx) do
      elem_name = :"#{Unifex.CodeGenerator.Utils.sanitize_var_name("#{var_name}")}_i"

      len_var_name = "#{var_name}_length"
      native_type = BaseType.generate_native_type(ctx.subtype, ctx.generator, ctx)
      %{subtype: subtype, postproc_fun: postproc_fun, generator: generator} = ctx

      ~g"""
      ({
      int get_list_length_result = enif_get_list_length(env, #{arg}, &#{len_var_name});
      if(get_list_length_result){
        #{var_name} = (#{native_type} *) enif_alloc(sizeof(#{native_type}) * #{len_var_name});

        for(unsigned int i = 0; i < #{len_var_name}; i++) {
          #{BaseType.generate_initialization(subtype, :"#{var_name}[i]", generator, ctx)}
        }

        ERL_NIF_TERM list = #{arg};
        for(unsigned int i = 0; i < #{len_var_name}; i++) {
          ERL_NIF_TERM elem;
          enif_get_list_cell(env, list, &elem, &list);
          #{native_type} #{elem_name} = #{var_name}[i];
          #{BaseType.generate_arg_parse(subtype, elem_name, ~g<elem>, postproc_fun, generator, ctx)}
          #{var_name}[i] = #{elem_name};
        }
      }
      get_list_length_result;
      })
      """
    end
  end

  defmodule CNode do
    @moduledoc false
    use Unifex.CodeGenerator.BaseType
    alias Unifex.CodeGenerator.BaseType

    @impl true
    def generate_arg_serialize(name, ctx) do
      ~g"""
      ({
        ei_x_encode_list_header(out_buff, #{name}_length);
        for(unsigned int i = 0; i < #{name}_length; i++) {
          #{BaseType.generate_arg_serialize(ctx.subtype, :"#{name}[i]", ctx.generator, ctx)}
        }
        ei_x_encode_empty_list(out_buff);
      });
      """
    end

    @impl true
    def generate_arg_parse(arg, var_name, ctx) do
      elem_name = :"#{var_name}[i]"
      len_var_name = "#{var_name}_length"
      native_type = BaseType.generate_native_type(ctx.subtype, ctx.generator, ctx)
      %{subtype: subtype, postproc_fun: postproc_fun, generator: generator} = ctx

      ~g"""
      ({
        int type;
        int size;

        ei_get_type(#{arg}->buff, #{arg}->index, &type, &size);
        #{len_var_name} = (unsigned int) size;

        int index = 0;
        UnifexCNodeInBuff unifex_buff;
        UnifexCNodeInBuff *unifex_buff_ptr = &unifex_buff;
        if(type == ERL_STRING_EXT) {
          ei_x_buff buff = unifex_cnode_string_to_list(#{arg}, #{len_var_name});
          unifex_buff.buff = buff.buff;
          unifex_buff.index = &index;
        } else {
          unifex_buff.buff = #{arg}->buff;
          unifex_buff.index = #{arg}->index;
        }
        int header_res = ei_decode_list_header(unifex_buff_ptr->buff, unifex_buff_ptr->index, &size);
        #{len_var_name} = (unsigned int) size;
        #{var_name} = (#{native_type} *)malloc(sizeof(#{native_type}) * #{len_var_name});

        for(unsigned int i = 0; i < #{len_var_name}; i++) {
          #{BaseType.generate_initialization(subtype, elem_name, generator, ctx)}
        }

        for(unsigned int i = 0; i < #{len_var_name}; i++) {
          #{BaseType.generate_arg_parse(subtype,
      elem_name,
      "unifex_buff_ptr",
      postproc_fun,
      generator,
      ctx)}
        }
        if(#{len_var_name}) {
          header_res = ei_decode_list_header(unifex_buff_ptr->buff, unifex_buff_ptr->index, &size);
        }
        header_res;
      })
      """
    end
  end
end

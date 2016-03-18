#ifndef STRUCTURE_H
#define STRUCTURE_H

#include <iostream>
#include <set>
#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "cereal/archives/json.hpp"
#include "cereal/types/vector.hpp"
#include <cereal/types/memory.hpp>

namespace llvmberry {

	enum TyScope { Source = 0, Target };

  std::string getBasicBlockIndex(const llvm::BasicBlock *block);
  std::string getVariable(const llvm::Value &value);
  bool name_instructions(llvm::Function &F);

  /* position */

	struct TyPositionPhinode {
	public:
		TyPositionPhinode(std::string _block_name, std::string _prev_block_name);
		void serialize(cereal::JSONOutputArchive &archive) const;

	private:
		std::string block_name;
		std::string prev_block_name;
	};

	struct TyPositionCommand {
	public:
		TyPositionCommand(enum TyScope _scope, std::string _register_name);
		void serialize(cereal::JSONOutputArchive &archive) const;

	private:
		enum TyScope scope;
		std::string register_name;
	};

	struct TyPosition {
	public:
		virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
	};

	struct ConsPhinode : public TyPosition {
	public:
		ConsPhinode(std::unique_ptr<TyPositionPhinode> _position_phinode);
		ConsPhinode(std::string _block_name, std::string _prev_block_name);
		void serialize(cereal::JSONOutputArchive &archive) const;

	private:
		std::unique_ptr<TyPositionPhinode> position_phinode;
		int n;
	};

	struct ConsCommand : public TyPosition {
	public:
		ConsCommand(std::unique_ptr<TyPositionCommand> _position_command);
		ConsCommand(enum TyScope _scope, std::string _register_name);
		void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyPosition> make(enum TyScope _scope, std::string _register_name);

	private:
		std::unique_ptr<TyPositionCommand> position_command;
	};

  // abstract
  struct TyNopPosition {
  public:
    virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  };

  struct ConsPhinodeCurrentBlockName : public TyNopPosition {
  public:
    ConsPhinodeCurrentBlockName(std::string _block_name);
    void serialize(cereal::JSONOutputArchive &archive) const;
    static std::unique_ptr<TyNopPosition> make(std::string _block_name);
  private:
    std::string block_name;
  };

  struct ConsCommandRegisterName : public TyNopPosition {
  public:
    ConsCommandRegisterName(std::string _register_name);
    void serialize(cereal::JSONOutputArchive &archive) const;
    static std::unique_ptr<TyNopPosition> make(std::string _register_name);
  private:
    std::string register_name;
  };

  struct ConsNopPosition : public TyNopPosition {
  public:
    ConsNopPosition(std::string _regname_or_blockname, bool _isPhi);
    void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyNopPosition> make(std::string _regname_or_blockname, bool _isPhi);

  private:
    bool isPhi;
    std::string regname_or_blockname;
  };

  /* value */

  // register

	enum TyTag { Physical = 0, Previous, Ghost };

	struct TyRegister {
	public:
		TyRegister(std::string _name, enum TyTag _tag);
		void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyRegister> make(std::string _name, enum TyTag _tag);

	private:
		std::string name;
		enum TyTag tag;
	};

  // constant

  struct TyIntType {
  public:
    virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  };

  struct ConsIntType : public TyIntType {
  public:
    ConsIntType(int _value);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    int value;
  };

  enum TyFloatType { FloatType = 0, DoubleType, FP128Type, X86_FP80Type };

  struct TyConstInt {
  public:
    TyConstInt(int _int_value, std::unique_ptr<TyIntType> _int_type);
    TyConstInt(int _int_value, int _value);
    void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyConstInt> make(int _int_value, int _value);

  private:
    int int_value;
    std::unique_ptr<TyIntType> int_type;
  };

  struct TyConstFloat {
  public:
    TyConstFloat(float _float_value, enum TyFloatType _float_type);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    float float_value;
    enum TyFloatType float_type;
  };

  struct TyConstant {
  public:
    virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  };

  struct ConsConstInt : public TyConstant {
  public:
    ConsConstInt(std::unique_ptr<TyConstInt> _const_int);
    ConsConstInt(int _int_value, int _value);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    std::unique_ptr<TyConstInt> const_int;
  };

  struct ConsConstFloat : public TyConstant {
  public:
    ConsConstFloat(std::unique_ptr<TyConstFloat> _const_float);
    ConsConstFloat(float _float_value, enum TyFloatType _float_type);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    std::unique_ptr<TyConstFloat> const_float;
  };

  // size

  struct TySize {
  public:
    virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  };

  struct ConsSize : public TySize {
  public:
    ConsSize(int _size);
    void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TySize> make(int _size);

  private:
    int size;
  };

  /* propagate */

  // propagate expression

	struct TyPropagateExpr {
	public:
		virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
	};

	struct ConsVar : public TyPropagateExpr {
	public:
		ConsVar(std::unique_ptr<TyRegister> _register_name);
		ConsVar(std::string _name, enum TyTag _tag);
		void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyPropagateExpr> make
      (std::string _name, enum TyTag _tag);

	private:
		std::unique_ptr<TyRegister> register_name;
	};

	struct ConsRhs : public TyPropagateExpr {
	public:
		ConsRhs(std::unique_ptr<TyRegister> _register_name);
		ConsRhs(std::string _name, enum TyTag _tag);
		void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyPropagateExpr> make
      (std::string _name, enum TyTag _tag);

	private:
		std::unique_ptr<TyRegister> register_name;
	};

  struct ConsConst : public TyPropagateExpr {
  public:
    ConsConst(std::unique_ptr<TyConstant> _constant);

    ConsConst(int _int_value, int _value);
    ConsConst(float _float_value, enum TyFloatType _float_type);

    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    std::unique_ptr<TyConstant> constant;
  };

  // propagate object

	struct TyPropagateLessdef {
	public:
		TyPropagateLessdef(std::unique_ptr<TyPropagateExpr> _lhs,
											 std::unique_ptr<TyPropagateExpr> _rhs,
											 enum TyScope _scope);
		void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyPropagateLessdef> make
    (std::unique_ptr<TyPropagateExpr> _lhs,
     std::unique_ptr<TyPropagateExpr> _rhs,
     enum TyScope _scope);

	private:
		std::unique_ptr<TyPropagateExpr> lhs;
		std::unique_ptr<TyPropagateExpr> rhs;
		enum TyScope scope;
	};

  struct TyPropagateNoalias {
  public:
    TyPropagateNoalias(std::unique_ptr<TyRegister> _lhs, std::unique_ptr<TyRegister> _rhs, enum TyScope _scope);
    TyPropagateNoalias(std::string _lhs_name, enum TyTag _lhs_tag,
                       std::string _rhs_name, enum TyTag _rhs_tag,
                       enum TyScope _scope);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    std::unique_ptr<TyRegister> lhs;
    std::unique_ptr<TyRegister> rhs;
    enum TyScope scope;
  };

	struct TyPropagateObject {
	public:
		virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
	};

	struct ConsLessdef : public TyPropagateObject {
	public:
		ConsLessdef(std::unique_ptr<TyPropagateLessdef> _propagate_lessdef);
		void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyPropagateObject> make
      (std::unique_ptr<TyPropagateExpr> _lhs,
       std::unique_ptr<TyPropagateExpr> _rhs,
       enum TyScope _scope);

	private:
		std::unique_ptr<TyPropagateLessdef> propagate_lessdef;
	};

  struct ConsNoalias : public TyPropagateObject {
  public:
    ConsNoalias(std::unique_ptr<TyPropagateNoalias> _propagate_noalias);
    ConsNoalias(std::string _lhs_name, enum TyTag _lhs_tag,
                std::string _rhs_name, enum TyTag _rhs_tag,
                enum TyScope _scope);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    std::unique_ptr<TyPropagateNoalias> propagate_noalias;
  };

	struct ConsMaydiff : public TyPropagateObject {
	public:
		ConsMaydiff(std::unique_ptr<TyRegister> _register_name);
		ConsMaydiff(std::string _name, enum TyTag _tag);
		void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyPropagateObject> make
      (std::unique_ptr<TyRegister> reg);

	private:
		std::unique_ptr<TyRegister> register_name;
	};

  // propagate range

	struct TyPropagateRange {
	public:
		virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
    virtual bool isGlobal(void) const { return false; }
	};

	struct ConsBounds : public TyPropagateRange {
	public:
		ConsBounds(std::unique_ptr<TyPosition> _from,
							 std::unique_ptr<TyPosition> _to);
		void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyPropagateRange> make
      (std::unique_ptr<TyPosition> _from,
       std::unique_ptr<TyPosition> _to);

	private:
		std::unique_ptr<TyPosition> from;
		std::unique_ptr<TyPosition> to;
	};

	struct ConsGlobal : public TyPropagateRange {
	public:
		ConsGlobal();
		void serialize(cereal::JSONOutputArchive &archive) const;
    virtual bool isGlobal(void) const { return true; }

    static std::unique_ptr<TyPropagateRange> make();
	};

	struct TyPropagate {
	public:
		TyPropagate(std::unique_ptr<TyPropagateObject> _propagate,
								std::unique_ptr<TyPropagateRange> _propagate_range);
		void serialize(cereal::JSONOutputArchive &archive) const;

	private:
		std::unique_ptr<TyPropagateObject> propagate;
		std::unique_ptr<TyPropagateRange> propagate_range;
	};

  /* inference rule */

  struct TyAddAssociative {
  public:
    TyAddAssociative(std::unique_ptr<TyRegister> _x,
                     std::unique_ptr<TyRegister> _y,
                     std::unique_ptr<TyRegister> _z,
                     std::unique_ptr<TyConstInt> _c1,
                     std::unique_ptr<TyConstInt> _c2,
                     std::unique_ptr<TyConstInt> _c3,
                     std::unique_ptr<TySize> _sz);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    std::unique_ptr<TyRegister> x;
    std::unique_ptr<TyRegister> y;
    std::unique_ptr<TyRegister> z;
    std::unique_ptr<TyConstInt> c1;
    std::unique_ptr<TyConstInt> c2;
    std::unique_ptr<TyConstInt> c3;
    std::unique_ptr<TySize> sz;
  };

  struct TySubAdd {
  public:
    TySubAdd(std::unique_ptr<TyRegister> _z,
             std::unique_ptr<TyRegister> _my,
             std::unique_ptr<TyRegister> _x,
             std::unique_ptr<TyRegister> _y,
             std::unique_ptr<TySize> _sz);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    std::unique_ptr<TyRegister> z;
    std::unique_ptr<TyRegister> my;
    std::unique_ptr<TyRegister> x;
    std::unique_ptr<TyRegister> y;
    std::unique_ptr<TySize> sz;
  };

  struct TyInfrule {
  public:
    virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  };

  struct ConsAddAssociative : TyInfrule {
  public:
    ConsAddAssociative(std::unique_ptr<TyAddAssociative> _add_associative);
    void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyInfrule> make
      (std::unique_ptr<TyRegister> _x,
       std::unique_ptr<TyRegister> _y,
       std::unique_ptr<TyRegister> _z,
       std::unique_ptr<TyConstInt> _c1,
       std::unique_ptr<TyConstInt> _c2,
       std::unique_ptr<TyConstInt> _c3,
       std::unique_ptr<TySize> _sz);

  private:
    std::unique_ptr<TyAddAssociative> add_associative;
  };

  struct ConsSubAdd : TyInfrule {
  public:
    ConsSubAdd(std::unique_ptr<TySubAdd> _sub_add);
    void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyInfrule> make
      (std::unique_ptr<TyRegister> _z,
       std::unique_ptr<TyRegister> _my,
       std::unique_ptr<TyRegister> _x,
       std::unique_ptr<TyRegister> _y,
       std::unique_ptr<TySize> _sz);

  private:
    std::unique_ptr<TySubAdd> sub_add;
  };

  /* hint command */

  struct TyCommand {
  public:
    virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  };

  struct ConsPropagate : public TyCommand {
  public:
    ConsPropagate(std::unique_ptr<TyPropagate> _propagate);
    void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyCommand> make
      (std::unique_ptr<TyPropagate> _propagate);
    static std::unique_ptr<TyCommand> make
      (std::unique_ptr<TyPropagateObject> _obj,
       std::unique_ptr<TyPropagateRange> _range);

  private:
    std::unique_ptr<TyPropagate> propagate;
  };

  struct ConsInfrule : public TyCommand {
  public:
    ConsInfrule(std::unique_ptr<TyPosition> _position,
                std::unique_ptr<TyInfrule> _infrule);
    void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyCommand> make
      (std::unique_ptr<TyPosition> _position,
       std::unique_ptr<TyInfrule> _infrule);

  private:
    std::unique_ptr<TyPosition> position;
    std::unique_ptr<TyInfrule> infrule;
  };

  /* core hint */

  struct CoreHint {
  public:
    CoreHint();
    CoreHint(std::string _module_id,
             std::string _function_id,
             std::string _opt_name);
    void addCommand(std::unique_ptr<TyCommand> c);
    void addSrcNopPosition(std::unique_ptr<TyNopPosition> position);
    void addTgtNopPosition(std::unique_ptr<TyNopPosition> position);
    void serialize(cereal::JSONOutputArchive &archive) const;

  private:
    std::string module_id;
    std::string function_id;
    std::string opt_name;
    std::vector<std::unique_ptr<TyNopPosition>> src_nop_positions;
    std::vector<std::unique_ptr<TyNopPosition>> tgt_nop_positions;
    std::vector<std::unique_ptr<TyCommand>> commands;
  };

} // llvmberry

#endif

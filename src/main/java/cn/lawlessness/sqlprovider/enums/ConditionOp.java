package cn.lawlessness.sqlprovider.enums;

/**
 * @author liutao
 * @date 2024/11/27
 */
public enum ConditionOp {

    NOTNULL("notnull"),
    ISNULL("isnull"),
    IN("in"),
    NOTIN("notin"),     // not contains -> [col] not null or [param] not contains [col]
    E_NOTIN("notin"),   // [col] exists and not in ->  [param] not contains [col]
    NE("ne"),   // not equals -> [col] is null or [col] <> [param]
    E_NE("e_ne"), // [col] exists and not equals -> [col] <> [param]
    EQ("eq"),
    LT("lt"),
    LE("le"),
    GT("gt"),
    GE("ge"),
    BETWEEN("between"),
    LIKE("like"),
    LEFT_LIKE("left_like"),
    RIGHT_LIKE("right_like"),
    DATE_EQ("date_eq"),
    ;
    private final String code;
    ConditionOp(String code) {
        this.code = code;
    }


    public String getCode() {
        return this.code;
    }

    public static ConditionOp getByOp(String op) {
        for (ConditionOp conditionOp : values()) {
            if (conditionOp.code.equals(op)) {
                return conditionOp;
            }
        }
        return null;
    }
}

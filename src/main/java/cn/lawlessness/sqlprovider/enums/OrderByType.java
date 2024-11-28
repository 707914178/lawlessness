package cn.lawlessness.sqlprovider.enums;


/**
 * @author liutao
 * @date 2023/9/21 14:38
 */
public enum OrderByType {

    ASC("asc", "asc"),
    DESC("desc", "desc"),
    ASCENDING("ascending", "asc"),
    DESCENDING("descending", "desc"),
    ;

    private String code;
    private String type;

    OrderByType(String code, String type) {
        this.code = code;
        this.type = type;
    }

    public String getCode() {
        return code;
    }

    public String getType() {
        return type;
    }

    public static String getOrderByType(String code) {
        if (null == code) {
            return null;
        }
        code = code.toLowerCase();
        for (OrderByType value : values()) {
            if (value.code.equals(code)) {
                return value.type;
            }
        }
        return null;
    }

}

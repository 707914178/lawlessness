package cn.lawlessness.sqlprovider.utils;

/**
 * @author liutao
 * @date 2023/9/18 20:39
 */
public class StringUtil {

    public static boolean isEmpty(CharSequence charSequence) {
        return null == charSequence || "".contentEquals(charSequence);
    }

    public static boolean isNotEmpty(CharSequence charSequence) {
        return !isEmpty(charSequence);
    }

}

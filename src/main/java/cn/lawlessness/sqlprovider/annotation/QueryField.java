package cn.lawlessness.sqlprovider.annotation;

import cn.lawlessness.sqlprovider.enums.ConditionOp;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author liutao
 * @date 2024/11/27
 */
@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
public @interface QueryField {

    String fieldId() default "";

    ConditionOp op() default ConditionOp.EQ;

    /**
     * 0: where
     * 1: having...
     * @return
     */
    int type() default 0;

}

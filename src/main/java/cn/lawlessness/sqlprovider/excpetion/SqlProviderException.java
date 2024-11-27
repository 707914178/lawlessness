package cn.lawlessness.sqlprovider.excpetion;

/**
 * @author liutao
 * @date 2024/11/27
 */
public class SqlProviderException extends RuntimeException {
    int code;

    public SqlProviderException() { }

    public SqlProviderException(String message) {
        super(message);
    }

    public SqlProviderException(int code) {
        this.code = code;
    }

    public SqlProviderException(int code, Throwable e) {
        super(e);
        this.code = code;
    }

    public SqlProviderException(Throwable e) {
        super(e);
    }


    public SqlProviderException(int code, String message) {
        super(message);
        this.code = code;
    }

    public SqlProviderException(int code, String message, Throwable e) {
        super(message, e);
        this.code = code;
    }

    public int getCode() {
        return this.code;
    }
}
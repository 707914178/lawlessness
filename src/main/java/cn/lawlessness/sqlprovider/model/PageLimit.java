package cn.lawlessness.sqlprovider.model;

/**
 * @author liutao
 * @date 2023/12/27 16:23
 */
public class PageLimit {

    public Integer pageIdx;

    public Integer pageSize;

    public static PageLimit ofSize(int pageSize) {
        PageLimit pageLimit = new PageLimit();
        pageLimit.pageSize = pageSize;
        return pageLimit;
    }

    public static PageLimit of(int pageIdx,int pageSize) {
        PageLimit pageLimit = new PageLimit();
        pageLimit.pageIdx =  pageIdx;
        pageLimit.pageSize = pageSize;
        return pageLimit;
    }

}
